;; init-packages.el --- Setup all packages
;;;; load packages

;;; Commentary:
;;

(require 'dired-x)

(use-package s :ensure t)
(use-package f :ensure t)
(use-package diminish
  :ensure t
  :commands (diminish))

;;; Code:

(use-package docker
  :ensure t
  :bind ("C-c d" . docker))

;; A few more useful configurations...
(use-package emacs
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Grow and shrink minibuffer
  (setq resize-mini-windows t)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t)
  ;; Enable tab completion
  (setq tab-always-indent 'complete))

(use-package dired-subtree
  :ensure t
  :after dired
  :config
  (bind-key "<tab>" #'dired-subtree-toggle dired-mode-map)
  (bind-key "<backtab>" #'dired-subtree-cycle dired-mode-map))

(use-package wgrep
  :ensure t
  :demand t
  :bind (("C-x C-q" . boem-change-to-writable-mode)))

(use-package neotree
  :commands (neotree)
  :ensure t
  :bind ("<f8>" . neotree-toggle))

(use-package discover-my-major
  :ensure t
  :commands (discover-my-major discover-my-mode))

(use-package docker-compose-mode
  :ensure t
  :commands (docker-compose-mode))
(use-package dockerfile-mode
  :ensure t
  :commands (dockerfile-mode))
(use-package fringe-helper
  :ensure t
  :after (flycheck))
(use-package smartrep :ensure t :defer t)
(use-package ov :ensure t :defer t)
(use-package restclient
  :ensure t
  :commands (restclient-mode))
(use-package wgrep
  :ensure t
  :commands (wgrep-change-to-wgrep-modex))
(use-package smex
  :ensure t
  :commands (smex))

;;;; savehist
(use-package savehist
  :hook (after-init . savehist-mode)
  :init
  (setq
   ;; save searh entries
   savehist-additional-variables '(search-ring regexp-search-ring extended-command-history)
   savehist-autosave-interval 60
   history-length 1000
   savehist-file (expand-file-name "savehist" boem-user-data-directory)))

;;;; avy
;; (use-package avy
;;   :ensure t
;;   :commands (avy-goto-char
;;              avy-goto-char-2
;;              avy-goto-line)
;;   :bind (("C-c j" . avy-goto-char)
;;          ("C-c J" . avy-goto-char-2)
;;          ("C-c l" . avy-goto-line))
;;   :config
;;   (progn
;;     (avy-setup-default)))

;; ;;;; anzu-mode
;; (use-package anzu
;;   :commands (anzu-mode)
;;   :ensure t
;;   :config (global-anzu-mode 1)
;;   :diminish "")

;;;; browse-kill-ring
(use-package browse-kill-ring
  :commands (browse-kill-ring)
  :ensure t
  :bind (("M-y" . browse-kill-ring)))

;;;; easy-kill
(use-package easy-kill
  :ensure t
  :commands easy-kill
  :config
  (progn
    (global-set-key [remap kill-ring-save] 'easy-kill)))

;;;; elisp-slime-nav - not using
;; (use-package elisp-slime-nav
;;   :ensure t
;;   :commands (elisp-slime-nav-mode)
;;   :diminish elisp-slime-nav-mode
;;   :config
;;   (progn
;;     (add-hook 'emacs-lisp-mode-hook (lambda () (elisp-slime-nav-mode t)))))

;;;; change-inner - not using
;; (use-package change-inner
;;   :ensure t
;;   :commands (change-inner
;;              change-outer)
;;   :bind (
;;          ("C-c C-i" . change-inner)
;;          ("C-c C-o" . change-outer)))

;; ;;;; flycheck - not using
;; (use-package flycheck
;;   :ensure t
;;   :commands (flycheck-mode
;;              global-flycheck-mode)
;;   :diminish ((global-flycheck-mode . "fc")
;;              (flycheck-mode . "fc"))
;;   :bind (("C-c e" . flycheck-list-errors))
;;   :config
;;   (progn
;;     (setq
;;      flycheck-highlighting-mode 'lines
;;      ;; flycheck-highlighting-mode 'symbols
;;      flycheck-completion-system 'ido)
;;     (defun flycheck-turn-on-maybe ()
;;       (unless (boem-current-buffer-remote-p)
;;         (flycheck-mode)))
;;     (add-hook 'python-mode-hook 'flycheck-turn-on-maybe)
;;     (add-hook 'js-ts-mode-hook 'flycheck-turn-on-maybe)
;;     (add-hook 'json-ts-mode-hook 'flycheck-turn-on-maybe)
;;     (add-hook 'ruby-ts-mode-hook 'flycheck-turn-on-maybe)
;;     (add-hook 'php-mode-hook 'flycheck-turn-on-maybe)
;;     (add-hook 'scss-mode-hook 'flycheck-turn-on-maybe)
;;     (add-hook 'haskell-mode-hook 'flycheck-turn-on-maybe)
;;     (add-hook 'elixir-ts-mode-hook 'flycheck-turn-on-maybe)

;;     (when (fboundp 'define-fringe-bitmap)
;;       (require 'fringe-helper)
;;       (fringe-helper-define 'vertical-wave-bitmap '(center repeat)
;;         "...XXX."
;;         "...XXX."
;;         "..XXX.."
;;         "..XXX..")

;;       (flycheck-define-error-level 'error
;;         :overlay-category 'flycheck-error-overlay
;;         :fringe-bitmap 'vertical-wave-bitmap
;;         :fringe-face 'flycheck-fringe-error)

;;       (flycheck-define-error-level 'warning
;;         :overlay-category 'flycheck-warning-overlay
;;         :fringe-bitmap 'vertical-wave-bitmap
;;         :fringe-face 'flycheck-fringe-warning)

;;       (flycheck-define-error-level 'info
;;         :overlay-category 'flycheck-info-overlay
;;         :fringe-bitmap 'vertical-wave-bitmap
;;         :fringe-face 'flycheck-fringe-info))))

;; ;;;; gist
;; (use-package gist
;;   :ensure t
;;   :commands (gist-region gist-buffer gist-region-or-buffer
;;                          gist-region-or-buffer-private gist-list))

;; ;;;; git-modes
;; (use-package git-modes
;;   :ensure t)

;;;; move-text
(use-package move-text
  :ensure t
  :commands (move-text-up move-text-down)
  :bind (("C-S-<up>" . move-text-up)
         ("M-S-<up>" . move-text-up)
         ("C-S-<down>" . move-text-down)
         ("M-S-<down>" . move-text-down)))

;; ;;;; which-func - not using
;; (use-package which-func
;;   :commands (which-func-mode)
;;   :init
;;   (progn
;;     (which-function-mode 1)))

;; (use-package ruby-electric - not using
;;   :disabled t
;;   :ensure t
;;   :commands ruby-electric-mode
;;   :diminish ruby-electric-mode
;;   :init
;;   (progn
;;     (unless (fboundp 'ruby-insert-end)
;;       (defun ruby-insert-end ()
;;         "Insert \"end\" at point and reindent current line."
;;         (interactive)
;;         (insert "end")
;;         (ruby-indent-line t)
;;         (end-of-line)))
;;     (setq
;;      ruby-block-highlight-toggle t
;;      ruby-block-delay 0.8)
;;     (add-hook 'ruby-ts-mode-hook 'ruby-electric-mode)))

;; (use-package yari
;;   :commands (yari)
;;   :ensure t)

;;;; Lisp

;; ;;;; rainbow-delimiters - not using
;; (use-package rainbow-delimiters
;;   :ensure t
;;   :commands rainbow-delimiters-mode
;;   :bind (("M-o m r" . rainbow-delimiters-mode)))

;; ;;;; Julia - not using
;; (use-package julia-mode
;;   :commands (julia-mode)
;;   :ensure t)

;; (use-package julia-shell
;;   :commands (julia-mode)
;;   :ensure t)

;; not using
;; ;; Fires up a separate buffer where you can navigate JSON (buffer or region)
;; (use-package json-navigator
;;   :ensure t)

;; ;;;; HTML

;; ;;;; web-mode - not using
;; (use-package web-mode
;;   :commands (web-mode)
;;   :ensure t
;;   :mode (("\\.phtml\\'" . web-mode) ("\\.erb\\'" . web-mode)
;;          ("\\.jsp\\'" . web-mode) ("\\.as[cp]x\\'" . web-mode)
;;          ("\\.blade\\.php\\'" . web-mode) ("\\.html\\'" . web-mode)
;;          ("\\.rhtml\\'" . web-mode) ("\\.mustache\\'" . web-mode)
;;          ("\\.hbs\\'" . web-mode)
;;          ("/\\(views\\|html\\|theme\\|templates\\)/.*\\.php\\'" . web-mode))
;;   :init
;;   (progn
;;     (setq
;;      web-mode-enable-auto-pairing nil
;;      web-mode-comment-style 2
;;      web-mode-indent-style 2
;;      web-mode-enable-whitespaces nil
;;      web-mode-enable-block-face t
;;      web-mode-enable-part-face t
;;      web-mode-enable-heredoc-fontification t
;;      web-mode-enable-comment-keywords t
;;      web-mode-enable-current-element-highlight t))
;;   :config
;;   (progn
;;     (bind-key "C-c ;" 'web-mode-comment-or-uncomment web-mode-map)
;;     (set-face-attribute 'web-mode-symbol-face nil :foreground "red")
;;     (unbind-key "C-c C-p" web-mode-map)
;;     (unbind-key "C-c C-n" web-mode-map)))

;; ;;;; haml-mode - not using
;; (use-package haml-mode
;;   :commands (haml-mode)
;;   :ensure t
;;   :mode (("\\.haml\\'" . haml-mode)))

;; ;;;; impatient-mode - not using
;; ;;;; Use it for live html editing preview
;; (use-package impatient-mode
;;   :commands (impatient-mode)
;;   :ensure t)

;;; Erlang
(cond
 ((string-equal system-type "darwin")
  (let ((mise-erl (shell-command-to-string "mise which erl")))
    (if (cl-search "command not found" mise-erl)
        (progn
          (add-to-list 'load-path "/Users/bosko/.asdf/installs/erlang/27.1.1/lib/tools-4.1/emacs/")
          (setq erlang-root-dir "/Users/bosko/.asdf/installs/erlang/27.1.1/")
          (setq exec-path (cons "/Users/bosko/.asdf/installs/erlang/27.1.1/bin" exec-path))
          (require 'erlang-start))
      (progn
        (setq erlang-root-dir (substring mise-erl 0 (cl-search "/bin/erl" mise-erl)))
        (setq exec-path (cons (concat erlang-root-dir "/bin") exec-path))
        (let ((tools-dir (file-expand-wildcards (concat erlang-root-dir "/lib/tools-*"))))
          (if tools-dir
              (add-to-list 'load-path (concat (car tools-dir) "/emacs")))
          )
        (require 'erlang-start)
        )
      )
    )
  (string-equal system-type "gnu/linux")
  (progn
    (add-to-list 'load-path "/usr/local/otp/lib/tools-<ToolsVer>/emacs")
    (setq erlang-root-dir "/usr/local/otp")
    (setq exec-path (cons "/usr/local/otp/bin" exec-path))
    (require 'erlang-start))))

;; ;;;; yasnippet
;; (use-package yasnippet
;;   :ensure t
;;   :commands (yas-minor-mode)
;;   :diminish yas-minor-mode
;;   :init
;;   (progn
;;     (setq ;; Yasnippet
;;      ;; Dont print yasnippet messages
;;      yas-verbosity 0
;;      ;; Snippet directories
;;      boem-yas-snippets-dir (expand-file-name
;;                             "snippets" user-emacs-directory)
;;      ;; Disable yasnippet prompt by default
;;      ;; (using auto-complete to prompt)
;;      yas-prompt-functions '(yas-popup-isearch-prompt
;;                             yas-ido-prompt yas-completing-prompt yas-no-prompt))
;;     (add-to-list 'auto-mode-alist
;;                  (cons
;;                   (concat (regexp-quote boem-yas-snippets-dir) ".*\\'")
;;                   'snippet-mode))
;;     (add-to-list 'auto-mode-alist
;;                  (cons
;;                   (concat (regexp-quote (file-truename
;;                                          boem-yas-snippets-dir)) ".*\\'")
;;                   'snippet-mode))

;;     (bind-key "C-x i" 'yas-insert-snippet)
;;     (add-hook 'prog-mode-hook #'yas-minor-mode))
;;   :config
;;   (progn
;;     (bind-key "C-x i" 'yas-insert-snippet yas-minor-mode-map)
;;     (use-package popup
;;       :ensure t
;;       :commands yas-popup-isearch-prompt
;;       :config
;;       (progn
;;         ;; FIXME this should be niced up and contributed back.
;;         (defun yas-popup-isearch-prompt (prompt choices &optional display-fn)
;;           (let ((group-max-len 0)
;;                 (key-max-len 0)
;;                 (fmt "")
;;                 (popup-items))

;;             (mapcar #'(lambda (choice)
;;                         (when (yas--template-p choice)
;;                           (setq group-max-len (max group-max-len
;;                                                    (+ (length (yas--template-group choice) )
;;                                                       (apply '+ (mapcar 'length (yas--template-group choice))))))
;;                           (setq key-max-len (max key-max-len (length (yas--template-key choice))))))
;;                     choices)

;;             (setq fmt (format "%s%%%d.%ds%s%%-%d.%ds│ %%s"
;;                               (if (> group-max-len 0 ) "" " ")
;;                               group-max-len group-max-len
;;                               (if (> group-max-len 0 ) " > " "")
;;                               key-max-len key-max-len))

;;             (setq popup-items
;;                   (mapcar
;;                    #'(lambda (choice)
;;                        (popup-make-item
;;                         (if (yas--template-p choice)
;;                             (format fmt
;;                                     (if (yas--template-group choice)
;;                                         (s-join "/" (yas--template-group choice))
;;                                       "")
;;                                     (if (yas--template-key choice)
;;                                         (yas--template-key choice)
;;                                       "")
;;                                     (if (yas--template-name choice)
;;                                         (yas--template-name choice)
;;                                       ""))
;;                           (format " %s" choice))
;;                         :value choice))
;;                    choices))

;;             (popup-menu*
;;              popup-items
;;              :prompt prompt
;;              :max-width 80
;;              :isearch t)))))

;;     (defun yas-remove-recompile-reload-all ()
;;       (interactive)
;;       (let ((default-directory boem-yas-snippets-dir) )
;;         (mapc (lambda (f)
;;                 (delete-file f))
;;               (file-expand-wildcards "*.elc")))
;;       (f-files boem-yas-snippets-dir
;;                (lambda (file)
;;                  (and
;;                   (equal (f-no-ext (f-filename file)) ".yas-compiled-snippets")
;;                   (f-delete file)))
;;                t)
;;       ;; (yas-recompile-all)
;;       (yas-reload-all))

;;     (defun my-snippet-save-hook ()
;;       (when (and buffer-file-name
;;                  (eq major-mode 'snippet-mode))
;;         (yas-remove-recompile-reload-all)))

;;     (defun my-snippet-mode-hook ()
;;       (add-hook 'after-save-hook 'my-snippet-save-hook nil t))
;;     (add-hook 'snippet-mode-hook 'my-snippet-mode-hook)

;;     (defun dired-snippets-dir ()
;;       "Open dired in the yas snippets dir."
;;       (interactive)
;;       (dired (expand-file-name
;;               "snippets" user-emacs-directory)))

;;     (yas-reload-all))
;;   )

;; (use-package yasnippet-snippets
;;   :ensure t
;;   :after (yasnippet))

;; ;;;; saveplace
;; (use-package saveplace
;;   :ensure t
;;   :config
;;   (progn
;;     (setq-default save-place t)))

;; ;;;; rbenv
;; (use-package rbenv
;;   :ensure t
;;   :commands (rbenv-use rbenv-use-global global-rbenv-mode)
;;   :init
;;   (progn
;;     (setq rbenv-installation-dir "/usr/local/")
;;     (global-rbenv-mode)))

;;;; ibuffer
(use-package ibuffer
  :defer t
  :init
  (progn
    (defvar boem-ibuffer-separator " • ")
    (setq ibuffer-filter-group-name-face 'variable-pitch
          ibuffer-use-header-line nil
          ibuffer-old-time 12)
    (require 'vc)
    (use-package ibuffer-vc
      :ensure t
      :commands
      (ibuffer-vc-set-filter-groups-by-vc-root
       ibuffer-vc-generate-filter-groups-by-vc-root)
      :config
      (progn
        (ibuffer-vc-set-filter-groups-by-vc-root)))
    (use-package ibuffer-tramp
      :ensure t
      :commands (ibuffer-tramp-generate-filter-groups-by-tramp-connection
                 ibuffer-tramp-set-filter-groups-by-tramp-connection))
    )
  :config
  (progn
    (unbind-key "M-o" ibuffer-mode-map)
    (bind-key "s" 'isearch-forward-regexp ibuffer-mode-map)
    (bind-key "." 'ibuffer-invert-sorting ibuffer-mode-map)

    (defun ibuffer-magit-status ()
      (interactive)
      (--when-let (get-buffer "*Ibuffer*")
        (with-current-buffer it
          (let* ((selected-buffer (ibuffer-current-buffer))
                 (buffer-path (with-current-buffer
                                  selected-buffer
                                (or (buffer-file-name)
                                    list-buffers-directory
                                    default-directory)))
                 (default-directory
                   (if (file-regular-p buffer-path)
                       (file-name-directory buffer-path)
                     buffer-path)))
            (magit-status default-directory)))))
    (bind-key "i" 'ibuffer-magit-status ibuffer-mode-map)
    (bind-key "G" 'ibuffer-magit-status ibuffer-mode-map)

    (use-package ibuffer-git
      :ensure t)
    (use-package ibuffer-vc
      :ensure t)

    (define-ibuffer-column name-strip
      (:inline t
               :header-mouse-map ibuffer-name-header-map
               :props
               ('mouse-face
                'highlight 'keymap ibuffer-name-map
                'ibuffer-name-column t
                'help-echo
                '(if tooltip-mode
                     "mouse-1: mark this buffer\nmouse-2: select this buffer\nmouse-3: operate on this buffer"
                   "mouse-1: mark buffer mouse-2: select buffer mouse-3: operate"))
               :summarizer
               (lambda (strings)
                 (let ((bufs (length strings)))
                   (cond ((zerop bufs) "No buffers")
                         ((= 1 bufs) "1 buffer")
                         (t (format "%s buffers" bufs))))))
      (propertize
       (s-left
        (or
         (s-index-of uniquify-separator (buffer-name))
         (string-width (buffer-name)))
        (buffer-name))
       'font-lock-face (ibuffer-buffer-name-face buffer mark)))

    (define-ibuffer-column size-h
      (:name "Size" :inline t)
      (cond
       ((> (buffer-size) 1000)
        (format "%7.1fk" (/ (buffer-size) 1000.0)))
       ((> (buffer-size) 1000000)
        (format "%7.1fM" (/ (buffer-size) 1000000.0)))
       (t
        (format "%8d" (buffer-size)))))

    (defvar ibuffer-magit-filter-groups nil)
    (defun ibuffer-magit-define-filter-groups ()
      (when (and (not ibuffer-magit-filter-groups)
                 (boundp 'magit-repo-dirs))
        (setq ibuffer-magit-filter-groups
              (--map (list
                      (concat "git:: "
                              (file-name-nondirectory (directory-file-name it)))
                      `(filename2 . ,it))
                     (mapcar 'cdr (magit-list-repos magit-repo-dirs))))))

    (defun ibuffer-set-filter-groups-by-root ()
      (interactive)
      (setq ibuffer-filter-groups
            (-concat
             '(("MORE"
                (or (mode . magit-log-edit-mode)
                    (name . "^\\*\\(traad-server\\|httpd\\|epc con.*\\|tramp/.*\\|Completions\\)\\*$")
                    (name . "^\\*Pymacs\\*$")
                    (name . "^\\*helm.*\\*")
                    (name . "^\\*Compile-log\\*$")
                    (name . "^\\*Ido Completions\\*$")
                    (name . "^\\*magit-\\(process\\)\\*$")
                    (name . "^ "))))
             '(("EMACS"
                (or
                 (name . "^\\*scratch")
                 (name . "^\\*Messages")
                 (name . "^\\*Help")
                 )))
             (ibuffer-vc-generate-filter-groups-by-vc-root)
             (ibuffer-tramp-generate-filter-groups-by-tramp-connection))))


    (defun toggle-ibuffer-filter-groups ()
      "DOCSTRING"
      (interactive)
      (let ((ibuf (get-buffer "*Ibuffer*")))
        (when ibuf
          (with-current-buffer ibuf
            (let ((selected-buffer (ibuffer-current-buffer)))
              (if (not ibuffer-filter-groups)
                  (ibuffer-set-filter-groups-by-root)
                (setq ibuffer-filter-groups nil))
              (pop-to-buffer ibuf)
              (ibuffer-update nil t)
              (ibuffer-jump-to-buffer (buffer-name selected-buffer )))))))
    (bind-key "h" 'toggle-ibuffer-filter-groups ibuffer-mode-map)

    (defun set-categorized-ibuffer-filter-group ()
      "DOCSTRING"
      (interactive)
      (let ((ibuf (get-buffer "*Ibuffer*")))
        (when ibuf
          (with-current-buffer ibuf
            (let ((selected-buffer (ibuffer-current-buffer)))
              (pop-to-buffer ibuf)
              (ibuffer-switch-to-saved-filter-groups "categorized")
              (ibuffer-update nil t)
              (ibuffer-jump-to-buffer (buffer-name selected-buffer )))))))

    (bind-key "H" 'set-categorized-ibuffer-filter-group ibuffer-mode-map)

    (setq
     ibuffer-default-sorting-mode 'recency
     ibuffer-eliding-string "…"
     ibuffer-compile-formats t
     ibuffer-git-column-length 4
     ibuffer-formats '(
                       (
                        mark
                        (size-h 9 -1 :right)
                        " "
                        (mode 4 4 :right :elide)
                        " "
                        read-only
                        modified
                        " "
                        (name-strip 25 25 :left :elide)
                        " "
                        (vc-status-mini 1 1)
                        " "
                        filename-and-process)
                       (mark " " (name 16 -1) " " filename))
     ibuffer-show-empty-filter-groups nil
     ibuffer-saved-filter-groups
     (quote (("flat")
             ("categorized"
              ;; -------------------------------------------------
              ;; programming languages #1
              ("code" (or
                       (mode . emacs-lisp-mode)
                       (mode . python-mode)
                       (mode . ruby-ts-mode)
                       (mode . elixir-ts-mode)
                       (mode . typescript-ts-mode)
                       (mode . js-mode)
                       (mode . actionscript-mode)
                       (mode . java-mode)
                       (mode . sh-mode)
                       (mode . haskell-mode)
                       (mode . html-mode)
                       (mode . web-mode)
                       (mode . haml-mode)
                       (mode . nxml-mode)
                       (mode . kivy-mode)
                       (mode . scss-mode)
                       (mode . sass-mode)
                       (mode . stylus-mode)
                       (mode . css-mode)))
              ;; -------------------------------------------------
              ;; configuration/data files
              ("conf" (or
                       (mode . json-ts-mode)
                       (mode . yaml-ts-mode)
                       (mode . conf-mode)))
              ;; -------------------------------------------------
              ;; text/notetaking/org
              ("org agenda" (mode . org-agenda-mode))
              ("org" (or
                      (mode . org-mode)
                      (name . "^\\*Calendar\\*$")
                      (name . "^diary$")))
              ("text misc" (or
                            (mode . text-mode)
                            (mode . rst-mode)
                            (mode . markdown-mode)))
              ;; -------------------------------------------------
              ;; media
              ("media" (or
                        (mode . image-mode)))
              ;; -------------------------------------------------
              ;; misc
              ("w3m" (mode . w3m-mode))
              ("scm" (or
                      (mode . magit-status-mode)
                      (mode . magit-log-mode)
                      (mode . vc-annotate-mode)))
              ("dired" (mode . dired-mode))
              ("help" (or
                       (mode . Info-mode)
                       (mode . help-mode)
                       (mode . Man-mode)
                       (name . "^\\*frequencies\\*$")
                       (name . "^\\*Smex: Unbound Commands\\*$")
                       (name . "^\\*Personal Keybindings\\*$")))
              ("weechat" (mode . weechat-mode))
              ;; -------------------------------------------------
              ;; *buffer* buffers
              ("*kite*" (name . "^\\*kite.*\\*"))
              ("MORE" (or (mode . magit-log-edit-mode)
                          (name . "^\\*\\(traad-server\\|httpd\\|epc con.*\\|tramp/.*\\|Completions\\)\\*$")
                          (name . "^\\*Pymacs\\*$")
                          (name . "^\\*helm.*\\*")
                          (name . "^\\*Compile-log\\*$")
                          (name . "^\\*Ido Completions\\*$")
                          (name . "^\\*magit-\\(process\\|commit\\)\\*$")
                          (name . "^ ")))
              ("*buffer*" (name . "\\*.*\\*"))))))
    (add-hook 'ibuffer-mode-hook
              #'(lambda ()
                  (setq ibuffer-hidden-filter-groups '("MORE"))
                  (ibuffer-update nil t)
                  (hl-line-mode 1)
                  (ibuffer-vc-set-filter-groups-by-vc-root)))
    (defun ibuffer-ido-find-file ()
      "Like `ido-find-file', but default to the directory of the buffer at point."
      (interactive
       (let ((default-directory (let ((buf (ibuffer-current-buffer)))
                                  (if (buffer-live-p buf)
                                      (with-current-buffer buf
                                        default-directory)
                                    default-directory))))
         (ido-find-file-in-dir default-directory))))
    (bind-key "C-x C-f" 'ibuffer-ido-find-file ibuffer-mode-map)))

;;;; AsciiDoc
;; (use-package adoc-mode
;;   :ensure t
;;   :commands adoc-mode
;;   :mode ("\\.adoc\\'" . adoc-mode))

;;;; textile-mode
;; (use-package textile-mode
;;   :ensure t
;;   :commands textile-mode
;;   :mode ("\\.textile\\'" . textile-mode))

;; (use-package ox-md
;;   :commands (org-md-export-as-markdown
;;              org-md-export-to-markdown
;;              org-md-export-block))

;; Log commands to separate buffer
;; Keep this disabled since I do not need
;; it all the time.
(use-package mwe-log-commands
  :ensure t
  :disabled t)

(use-package websocket
  :ensure t
  :commands (websocket-open))

(provide 'init-packages)

;;; init-packages.el ends here
