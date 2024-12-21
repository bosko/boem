;;; init-basic.el --- Basic configurations

;;; Commentary:
;;

;;; Code:

(defvar boem-current-user
  (getenv (if (equal system-type 'windows-nt) "USERNAME" "USER")))

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

;; Stop hl-line interfering with default face suggested by
;; customize-face (taken
;; from https://sachachua.com/blog/2024/09/highlight-the-current-line-while-still-being-able-to-easily-customize-describe-underlying-faces/)
(defun boem-suggest-other-faces (func &rest args)
  (if global-hl-line-mode
      (progn
        (global-hl-line-mode -1)
        (prog1 (apply func args)
          (global-hl-line-mode 1)))
    (apply func args)))
(advice-add #'face-at-point :around #'boem-suggest-other-faces)

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
    ruby-mode-hook
    elixir-mode-hook
    sass-mode-hook))

(defun boem-current-buffer-remote-p ()
  (--any? (and it (file-remote-p it))
          (list
           (buffer-file-name)
           list-buffers-directory
           default-directory)))

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
  "Switch to previously opened buffer.
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

(defun boem-restclient ()
  (interactive)
  (let (($buf (generate-new-buffer "*Restclient*")))
    (switch-to-buffer $buf)
    (restclient-mode)))

(defvar boem-org-tags '(("Project" . ?p)
                        ("Article" . ?a)
                        ("Video" . ?v)
                        ("Book" . ?b)
                        ("Code" . ?c)
                        ("Encrypt". ?e))
  "Override this value by creating .boem-org-tags.el file in your home directory.")

(defun boem-change-to-writable-mode ()
  (interactive)
  (if (eq 'grep-mode (buffer-local-value 'major-mode (current-buffer)))
      (wgrep-change-to-wgrep-mode)
    (wdired-change-to-wdired-mode)))

(defvar boem-custom-daily-agenda
  ;; NOTE 2021-12-08: Specifying a match like the following does not
  ;; work.
  ;;
  ;; tags-todo "+PRIORITY=\"A\""
  ;;
  ;; So we match everything and then skip entries with
  ;; `org-agenda-skip-function'.
  `((tags-todo "*"
               ((org-agenda-skip-function
                 `(org-agenda-skip-entry-if
                   'notregexp ,(format "\\[#%s\\]" (char-to-string org-priority-highest))))
                (org-agenda-block-separator nil)
                (org-agenda-overriding-header "Важни задаци\n")))
    (agenda "" ((org-agenda-time-grid nil)
                (org-agenda-start-on-weekday nil)
                (org-agenda-span 1)
                (org-agenda-show-all-dates nil)
                (org-scheduled-past-days 365)
                ;; Excludes today's scheduled items
                (org-scheduled-delay-days 1)
                (org-agenda-block-separator nil)
                (org-agenda-entry-types '(:scheduled))
                (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                (org-agenda-day-face-function (lambda (date) 'org-agenda-date))
                (org-agenda-format-date "")
                (org-agenda-overriding-header "\nПрошли неурађени задаци")))
    (agenda "" ((org-agenda-span 1)
                (org-deadline-warning-days 0)
                (org-agenda-block-separator nil)
                (org-scheduled-past-days 0)
                ;; We don't need the `org-agenda-date-today'
                ;; highlight because that only has a practical
                ;; utility in multi-day views.
                (org-agenda-day-face-function (lambda (date) 'org-agenda-date))
                (org-agenda-overriding-header "\nДанашњи распоред\n")))
    (agenda "" ((org-agenda-start-on-weekday nil)
                (org-agenda-start-day "+1d")
                (org-agenda-span 5)
                (org-deadline-warning-days 0)
                (org-agenda-block-separator nil)
                (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                (org-agenda-overriding-header "\nНаредних пет дана\n")))
    (agenda "" ((org-agenda-time-grid nil)
                (org-agenda-start-on-weekday nil)
                ;; We don't want to replicate the previous section's
                ;; three days, so we start counting from the day after.
                (org-agenda-start-day "+4d")
                (org-agenda-span 14)
                (org-agenda-show-all-dates nil)
                (org-deadline-warning-days 0)
                (org-agenda-block-separator nil)
                (org-agenda-entry-types '(:deadline))
                (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                (org-agenda-overriding-header "\nПредстојећи рокови (+14д)\n"))))
  "Custom agenda for use in `org-agenda-custom-commands'.")

(provide 'init-basic)

;;; init-basic.el ends here
