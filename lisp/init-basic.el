;;; init-basic.el --- Basic configurations

;;; Commentary:
;;

;;; Code:

(defconst boem-version-string
  (mapconcat 'identity
             (mapcar
              #'(lambda(x) (number-to-string x))
              (version-to-list emacs-version))
             ".")
  "Emacs version as string.")

(defconst boem-user-package-directory
  (expand-file-name (format "packages/%s" boem-version-string) boem-init-root))
(defconst boem-user-data-directory
  (expand-file-name "data" boem-init-root))
(defconst boem-user-themes-directory
  (expand-file-name "themes" boem-init-root))
(defconst boem-user-org-directory
  (expand-file-name "~/org-files"))

(make-directory boem-user-package-directory t)
(make-directory boem-user-data-directory t)
(make-directory boem-user-themes-directory t)
(make-directory boem-user-org-directory t)

(defun boem-install-package-if-needed (package &optional min-version no-refresh)
  "Install required PACKAGE, optionally requiring MIN-VERSION.
If package is not found in the available package list or NO-REFRESH
is true refresh is skipped"
  (if (package-installed-p package)
      t
    (if (or (assoc package package-archive-contents) no-refresh)
        (package-install package)
      (progn
        (package-refresh-contents)
        (boem-install-package-if-needed package min-version t)))))

(defun boem-get-buffer-project-folder (buffer)
  "Returns folder for Git project"
  (let ((proj-folder (locate-dominating-file (buffer-file-name buffer) ".git")))
    (if proj-folder
        proj-folder)))

(defun boem-pop-eshell-bottom ()
  "Opens eshell buffer on the bottom of the window"
  (interactive)
  (with-current-buffer (current-buffer)
    (let ((cur-buff (current-buffer))
          (esh-buf (get-buffer "*bottom-eshell*")))
      (unless esh-buf
        (setq esh-buf (eshell 919))
        (rename-buffer "*bottom-eshell*")
        (switch-to-buffer cur-buff))
      (display-buffer-in-side-window esh-buf '((side . bottom)))
      (select-window (get-buffer-window esh-buf))
      (with-current-buffer "*bottom-eshell*"
        (cd (or (boem-get-buffer-project-folder cur-buff) "."))
        (eshell-emit-prompt)
        (eshell/clear-scrollback)
        (eshell-emit-prompt)))))

(defun boem-add-subdirs-to-load-path (root-dir)
  "Add all first lever sub directories of ROOT-DIR to load path."
  (dolist (entry (directory-files root-dir t "\\w+"))
    (when (file-directory-p entry)
      (if (string-match "theme" entry)
          (add-to-list 'custom-theme-load-path entry)
        (add-to-list 'load-path entry)))))

(boem-add-subdirs-to-load-path boem-user-package-directory)

;; My themes
(add-to-list 'custom-theme-load-path boem-user-themes-directory)

;;;; rename-modeline
(defmacro boem-rename-modeline (package-name mode new-name)
  `(eval-after-load ,package-name
     '(defadvice ,mode (after boem-rename-modeline activate)
        (setq mode-name ,new-name))))

;;;; Modes and mode groupings
(defmacro boem-hook-into-modes (func modes)
  "Add hook `FUNC' to multiple `MODES'."
  `(dolist (mode-hook ,modes)
     (add-hook mode-hook ,func)))

(defvar boem-prog-mode-hooks
  '(prog-mode-hook
    emacs-lisp-mode-hook
    pyhon-mode-hook
    js-mode-hook
    js2-mode-hook
    ruby-mode-hook
    elixir-mode-hook
    sass-mode-hook))

(defun boem-current-buffer-remote-p ()
  (--any? (and it (file-remote-p it))
          (list
           (buffer-file-name)
           list-buffers-directory
           default-directory))
  ;; (and (fboundp 'tramp-tramp-file-p) (-any? 'tramp-tramp-file-p
  ;; (list
  ;; (buffer-file-name)
  ;; list-buffers-directory
  ;; default-directory)))
  )

(defun boem-insert-line-above ()
  "Insert and indent line above current point."
  (interactive)
  (move-beginning-of-line nil)
  (newline)
  (forward-line -1)
  (indent-according-to-mode))

(defun boem-insert-line ()
  "Insert and indent line above current point."
  (interactive)
  (move-end-of-line nil)
  (newline)
  (indent-according-to-mode))

(defun boem-open-term ()
  "Open 'ansi-term' with default shell."
  (interactive)
  (ansi-term (getenv "SHELL")))

(defun boem-duplicate-line-or-region ()
  (interactive)
  (let ((string "")  (end nil))
    (if (use-region-p)
        (progn
          (setq end (region-end))
          (setq string (buffer-substring-no-properties (region-beginning) end))
          (goto-char end))
      (progn
        (setq end (line-end-position)
              string (buffer-substring-no-properties (line-beginning-position) end))
        (goto-char end)
        (newline)))
    (insert string)))

(defun boem-kill-user-buffers ()
  "Kills all opened buffers except *scratch* and *Messages*"
  (interactive)
  (let ((not-to-kill-buffer-list '("*scratch*" "*Messages*")))
    (dolist (buff (buffer-list))
      (if (and
           (not (s-starts-with? " " (buffer-name buff)))
           (not (member (buffer-name buff) not-to-kill-buffer-list)))
          (kill-buffer (buffer-name buff))))))

(defun boem-comment-uncomment ()
  (interactive)
  (save-excursion
    (if (not (region-active-p))
        (progn
          (beginning-of-line)
          (push-mark)
          (end-of-line)))
    (call-interactively 'comment-or-uncomment-region)))

;; Add Imenu index to the menu bar in any mode that supports Imenu.
(defun boem-try-to-add-imenu ()
  (condition-case nil (imenu-add-to-menubar "Methods") (error nil)))
(add-hook 'font-lock-mode-hook 'boem-try-to-add-imenu)

(defun boem-switch-to-previous-buffer ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers.
Code from: http://emacsredux.com/blog/2013/04/28/switch-to-previous-buffer/"
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(defun boem-set-proper-sql-prompt-regex ()
  (if (string= sql-product "postgres")
      (setq sql-prompt-regexp "[_[:alpha:]]*[=][#]")))

(defun boem-reopen-file-as-sudo ()
  (interactive)
  (when-let ((p (point)))
    (when-let ((file-name (buffer-file-name)))
      (find-alternate-file (concat "/sudo::" file-name)))
    (goto-char p)))

(defun boem/restclient ()
  (interactive)
  (let (($buf (generate-new-buffer "*Restclient*")))
    (switch-to-buffer $buf)
    (restclient-mode)))

(defvar boem-org-tags '(("Project" . ?p) ("Article" . ?a) ("Book" . ?b) ("Code" . ?c) ("Encrypt". ?e))
  "Override this value by creating .boem-org-tags.el file in your home directory.")

(provide 'init-basic)

;;; init-basic.el ends here
