;; init-packages.el --- Setup all packages
;;;; load packages

;;; Commentary:
;;

(require 'dired-x)

(use-package dash
  :ensure t
  :defer t
  :init
  (progn
    (eval-after-load 'dash '(dash-enable-font-lock))))

(use-package s :ensure t)
(use-package f :ensure t)
(use-package diminish
  :ensure t
  :commands (diminish))

;;; Code:

(use-package treesit-auto
  :ensure t
  :demand t
  :config
  (setq treesit-auto-install 'prompt)
  (global-treesit-auto-mode))

(use-package exec-path-from-shell
  :ensure t
  :commands (exec-path-from-shell-initialize)
  :init
  (progn
    (when (memq window-system '(mac ns x))
      (exec-path-from-shell-initialize))))

(use-package eat
  :ensure t
  :config
  (progn
    (add-hook 'eshell-load-hook #'eat-eshell-mode)
    (add-hook 'eshell-load-hook #'eat-eshell-visual-command-mode)
    (add-hook 'eat-mode-hook (lambda() (setq show-trailing-whitespace nil)))
    (eat-eshell-mode 1)))

(use-package docker-cli
  :commands (docker-cli)
  :ensure t)

(use-package docker
  :ensure t
  :bind ("C-c d" . docker))

(use-package popper
  :ensure t
  :bind (("M-§"   . popper-toggle-latest)
         ("C-§"   . popper-cycle)
         ("C-M-§" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "\\*eat\\*"
          "\\*emacs-eshell\\*"
          "^\\*eshell.*\\*$" eshell-mode
          help-mode))
  (popper-mode +1)
  (popper-echo-mode +1))

(use-package dash-docs
  :ensure t
  :init
  (setq dash-docs-enable-debugging nil)
  (setq dash-docs-browser-func #'eww)
  :config
  (mapc #'(lambda (doc) (dash-docs-activate-docset doc)) (dash-docs-installed-docsets)))

;; Enable vertico
(use-package vertico
  :ensure t
  :init
  (setq vertico-multiform-categories
      '((consult-grep buffer)
        (embark-keybinding grid)))
  (vertico-multiform-mode)
  (vertico-mode)
  :custom
  (vertico-sort-function 'vertico-sort-history-alpha)
  )

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

(use-package orderless
  :ensure t
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion))
                                        (eglot (styles orderless)))))

(use-package marginalia
  :ensure t
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

;;;; embark
(use-package embark
  :ensure t
  :after (which-key)
  :init
  (setq embark-indicators
      '(embark-minimal-indicator  ; default is embark-mixed-indicator
        embark-highlight-indicator
        embark-isearch-highlight-indicator)
      embark-prompter 'embark-completing-read-prompter)
  :bind (("C-c ." . embark-act)
         ("C-c ," . embark-act-noexit))
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))
  (customize-set-variable 'embark-verbose-indicator-display-action
                          '(display-buffer-at-bottom (window-height . fit-window-to-buffer))))

;;;; consult
(use-package consult
  ;; Replace bindings. Lazily loaded by `use-package'.
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-dash)                  ;; Alternative: consult-fd
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Tweak the register preview for `consult-register-load',
  ;; `consult-register-store' and the built-in commands.  This improves the
  ;; register formatting, adds thin separator lines, register sorting and hides
  ;; the window mode line.
  (advice-add #'register-preview :override #'consult-register-window)
  (setq register-preview-delay 0.5)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  (if (executable-find "rg")
      (bind-key "C-c c k" 'consult-ripgrep)
    (bind-key "C-c c k" 'consult-git-grep))

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key nil
   consult-ripgrep consult-grep consult-git-grep consult-man
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   :preview-key "M-.")

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (keymap-set consult-narrow-map (concat consult-narrow-key " ?") #'consult-narrow-help)
  )

;; Optionally add the `consult-flycheck' command.
(use-package consult-flycheck
  :bind (:map flycheck-command-map
              ("!" . consult-flycheck)))

(use-package consult-dash
  :ensure t
  :bind (("M-s d" . consult-dash))
  :config
  ;; Use the symbol at point as initial search term
  (consult-customize consult-dash :initial (thing-at-point 'symbol)))

(use-package embark-consult
  :ensure t
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . embark-consult-preview-at-point-mode))

(use-package wgrep
  :ensure t
  :demand t
  :bind (("C-x C-q" . boem-change-to-writable-mode)))

(use-package neotree
  :commands (neotree)
  :ensure t
  :bind ("<f8>" . neotree-toggle))

(use-package nerd-icons
  :ensure t)

(use-package nerd-icons-ibuffer
  :ensure t
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

(use-package nerd-icons-dired
  :ensure t
  :hook
  (dired-mode . nerd-icons-dired-mode))

(use-package nerd-icons-completion
  :ensure t
  :after marginalia
  :config
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

(use-package nerd-icons-corfu
  :ensure t
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

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

(use-package tramp
  :defer t
  :config
  (setq vc-ignore-dir-regexp
        (format "\\(%s\\)\\|\\(%s\\)"
                vc-ignore-dir-regexp
                tramp-file-name-regexp))
  (tramp-set-completion-function
   "ssh" (append (tramp-get-completion-function "ssh")
                 (mapcar (lambda (file) `(tramp-parse-sconfig ,file))
                         (directory-files
                          "~/.ssh"
                          'full directory-files-no-dot-files-regexp))))
  (tramp-set-completion-function
   "scp" (append (tramp-get-completion-function "scp")
                 (mapcar (lambda (file) `(tramp-parse-sconfig ,file))
                         (directory-files
                          "~/.ssh"
                          'full directory-files-no-dot-files-regexp)))))

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

;;;; whitespace
(use-package whitespace
  :commands (whitespace-mode)
  :defer t
  :bind (("C-c w" . whitespace-cleanup))
  :init
  (progn
    (add-hook 'before-save-hook 'whitespace-cleanup)))

;;;; avy
(use-package avy
  :ensure t
  :commands (avy-goto-char
             avy-goto-char-2
             avy-goto-line)
  :bind (("C-c j" . avy-goto-char)
         ("C-c J" . avy-goto-char-2)
         ("C-c l" . avy-goto-line))
  :config
  (progn
    (avy-setup-default)))

;;;; anzu-mode
(use-package anzu
  :commands (anzu-mode)
  :ensure t
  :config (global-anzu-mode 1)
  :diminish "")

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

;;;; elisp-slime-nav
(use-package elisp-slime-nav
  :ensure t
  :commands (elisp-slime-nav-mode)
  :diminish elisp-slime-nav-mode
  :config
  (progn
    (add-hook 'emacs-lisp-mode-hook (lambda () (elisp-slime-nav-mode t)))))

;;;; expand-region
(use-package expand-region
  :ensure t
  :commands (er/expand-region
             er/contract-region)
  :bind (("C-=" . er/expand-region)))

;;;; change-inner
(use-package change-inner
  :ensure t
  :commands (change-inner
             change-outer)
  :bind (
         ("C-c C-i" . change-inner)
         ("C-c C-o" . change-outer)))

;;;; view large files
(use-package vlf
  :commands (vlf vlf-mode)
  :ensure t
  :config
  (progn
    (require 'json))
  )

;;;; flycheck
(use-package flycheck
  :ensure t
  :commands (flycheck-mode
             global-flycheck-mode)
  :diminish ((global-flycheck-mode . "fc")
             (flycheck-mode . "fc"))
  :bind (("C-c e" . flycheck-list-errors))
  :config
  (progn
    (setq
     flycheck-highlighting-mode 'lines
     ;; flycheck-highlighting-mode 'symbols
     flycheck-completion-system 'ido)
    (defun flycheck-turn-on-maybe ()
      (unless (boem-current-buffer-remote-p)
        (flycheck-mode)))
    (add-hook 'python-mode-hook 'flycheck-turn-on-maybe)
    (add-hook 'js-ts-mode-hook 'flycheck-turn-on-maybe)
    (add-hook 'json-ts-mode-hook 'flycheck-turn-on-maybe)
    (add-hook 'ruby-ts-mode-hook 'flycheck-turn-on-maybe)
    (add-hook 'php-mode-hook 'flycheck-turn-on-maybe)
    (add-hook 'scss-mode-hook 'flycheck-turn-on-maybe)
    (add-hook 'haskell-mode-hook 'flycheck-turn-on-maybe)
    (add-hook 'elixir-ts-mode-hook 'flycheck-turn-on-maybe)

    (when (fboundp 'define-fringe-bitmap)
      (require 'fringe-helper)
      (fringe-helper-define 'vertical-wave-bitmap '(center repeat)
        "...XXX."
        "...XXX."
        "..XXX.."
        "..XXX..")

      (flycheck-define-error-level 'error
        :overlay-category 'flycheck-error-overlay
        :fringe-bitmap 'vertical-wave-bitmap
        :fringe-face 'flycheck-fringe-error)

      (flycheck-define-error-level 'warning
        :overlay-category 'flycheck-warning-overlay
        :fringe-bitmap 'vertical-wave-bitmap
        :fringe-face 'flycheck-fringe-warning)

      (flycheck-define-error-level 'info
        :overlay-category 'flycheck-info-overlay
        :fringe-bitmap 'vertical-wave-bitmap
        :fringe-face 'flycheck-fringe-info))))

;;;; gist
(use-package gist
  :ensure t
  :commands (gist-region gist-buffer gist-region-or-buffer
                         gist-region-or-buffer-private gist-list))

;;;; git-modes
(use-package git-modes
  :ensure t)

;;;; git-timemachine
(use-package git-timemachine
  :ensure t
  :commands (git-timemachine))

;;;; magit
(use-package magit
  :ensure t
  :commands (magit-log magit-blame magit-status magit-git-repo-p magit-list-repos)
  :bind (("C-x g" . magit-status))
  :init
  (progn
    (setq
     magit-repo-dirs-depth 4
     magit-status-buffer-switch-function 'switch-to-buffer
     magit-save-some-buffers t
     magit-log-author-date-max-length 25
     magit-log-auto-more t)
    (use-package magit-blame
      :commands magit-blame-mode)
    (add-hook 'git-commit-mode-hook
              #'(lambda ()
                  (auto-fill-mode)
                  )))
  :config
  (progn
    (require 'json)
    (bind-key "q" 'previous-buffer magit-status-mode-map)
    (bind-key "h" 'ibuffer magit-status-mode-map)
    (defun magit-toggle-whitespace ()
      (interactive)
      (if (member "-w" magit-diff-options)
          (magit-dont-ignore-whitespace)
        (magit-ignore-whitespace)))
    (defun magit-ignore-whitespace ()
      (interactive)
      (add-to-list 'magit-diff-options "-w")
      (magit-refresh))
    (defun magit-dont-ignore-whitespace ()
      (interactive)
      (setq magit-diff-options (remove "-w" magit-diff-options))
      (magit-refresh))
    (bind-key "W" 'magit-toggle-whitespace magit-status-mode-map)

    (transient-define-prefix th/magit-aux-commands
      ()
      "My personal auxiliary magit commands."
      ["Auxiliary commands"
       ("d" "Difftastic Diff (dwim)" th/magit-diff-with-difftastic)
       ("s" "Difftastic Show" th/magit-show-with-difftastic)])

    (transient-append-suffix 'magit-dispatch "!"
      '("#" "My Magit Cmds" th/magit-aux-commands))

    (define-key magit-status-mode-map (kbd "#") #'th/magit-aux-commands)))

;;;; diff-hl
(use-package diff-hl
  :ensure t
  :config
  (global-diff-hl-mode)
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode)
  (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  ;; Fall back to the display margin, if the fringe is unavailable
  (unless (display-graphic-p) (diff-hl-margin-mode)))

;;;; move-text
(use-package move-text
  :ensure t
  :commands (move-text-up move-text-down)
  :bind (("C-S-<up>" . move-text-up)
         ("M-S-<up>" . move-text-up)
         ("C-S-<down>" . move-text-down)
         ("M-S-<down>" . move-text-down)))

;;;; smartparens
(use-package smartparens
  :ensure t
  :commands (smartparens-mode smartparens-global-mode turn-on-smartparens-mode
                              turn-off-smartparens-mode show-smartparens-mode
                              show-smartparens-global-mode
                              smartparens-global-strict-mode
                              smartparens-strict-mode
                              turn-on-smartparens-strict-mode)
  :diminish ""
  :init
  (progn
    (require 'smartparens-config)
    (setq
     sp-show-pair-delay 0.125
     sp-show-pair-from-inside nil))
  :config
  (progn
    (bind-key "C-x C-r" 'sp-rewrap-sexp smartparens-mode-map)
    (sp-pair "<%" "%>" :wrap "C-%")
    (setq
     sp-ignore-modes-list '(calc-mode dired-mode ibuffer-mode
                                      minibuffer-inactive-mode sr-mode)
     sp-autoescape-string-quote nil)
    (sp-pair "'" nil :unless '(sp-point-after-word-p))
    (smartparens-global-mode t)
    (show-smartparens-global-mode t)))

;;;; undo-tree
(use-package undo-tree
  :ensure t
  :commands (global-undo-tree-mode turn-on-undo-tree-mode)
  :diminish undo-tree-mode
  :init
  (progn
    (setq
     undo-tree-visualizer-timestamps t
     undo-tree-history-directory-alist
     (list (cons "." (expand-file-name
                      (concat "undo-tree-save/" boem-current-user "/")
                      boem-user-data-directory))))

    ;; TODO undo-tree-save-history must not write to messages buffer
    ;; (unless (string< emacs-version "24.3")
    (setq undo-tree-auto-save-history t)

    (global-undo-tree-mode)
    ))

;;;; volatile-highlights
(use-package volatile-highlights
  :ensure t
  :commands volatile-highlights-mode
  :diminish volatile-highlights-mode
  :init
  (progn
    (defun turn-on-volatile-highlights-mode ()
      (volatile-highlights-mode 1))
    (boem-hook-into-modes #'turn-on-volatile-highlights-mode
                          boem-prog-mode-hooks)))

;;;; recentf
(use-package recentf
  :commands (recentf-mode recentf recentf-save-list recentf-load-list)
  :init
  (progn
    (setq
     recentf-max-saved-items 500
     recentf-max-menu-items 15
     recentf-save-file (expand-file-name "recentf" boem-user-data-directory)
     recentf-auto-cleanup 'never)
    (setq recentf-exclude '("recentf" "/packages/" "\\.ido.last" "COMMIT_EDITMSG"
                            "TAGS" "ido.hist" ".gz" "sql-out-*"))
    (recentf-mode 1))
  )

(use-package corfu
  :ensure t
  :hook (after-init . global-corfu-mode)
  :bind (:map corfu-map ("<tab>" . corfu-complete))
  :config
  (setq tab-always-indent 'complete)
  (setq corfu-preview-current nil)
  (setq corfu-min-width 20)

  (setq corfu-popupinfo-delay '(1.25 . 0.5))
  (corfu-popupinfo-mode 1) ; shows documentation after `corfu-popupinfo-delay'

  ;; Sort by input history (no need to modify `corfu-sort-function').
  (with-eval-after-load 'savehist
    (corfu-history-mode 1)
    (add-to-list 'savehist-additional-variables 'corfu-history))

  :init
  (use-package corfu-terminal
    :ensure t)

  (unless (display-graphic-p)
    (corfu-terminal-mode +1)))

;;;; which-func
(use-package which-func
  :commands (which-func-mode)
  :init
  (progn
    (which-function-mode 1)))

;;;; Ruby

;;;; ruby-mode
(use-package ruby-ts-mode
  :commands ruby-ts-mode
  :init
  (progn
    (setq
     ;; Avoid default, ugly, Ruby indentation
     ruby-deep-indent-paren nil)
    (eval-after-load "hideshow"
      '(add-to-list 'hs-special-modes-alist
                    `(ruby-ts-mode
                      ,(rx (or "def" "class" "module" "do" "{" "[" "if" "else" "unless")) ; Block start
                      ,(rx (or "}" "]" "end"))                       ; Block end
                      ,(rx (or "#" "=begin"))                        ; Comment start
                      forward-sexp nil)))
    (add-hook 'ruby-ts-mode-hook (lambda() (hs-minor-mode)))))

(use-package inf-ruby
  :ensure t
  :commands ruby-ts-mode
  :config
  (progn
    (inf-ruby-minor-mode +1))
  :init
  (progn
    (add-hook 'inf-ruby-mode-hook (lambda() (setq show-trailing-whitespace nil)))))

(use-package ruby-electric
  :disabled t
  :ensure t
  :commands ruby-electric-mode
  :diminish ruby-electric-mode
  :init
  (progn
    (unless (fboundp 'ruby-insert-end)
      (defun ruby-insert-end ()
        "Insert \"end\" at point and reindent current line."
        (interactive)
        (insert "end")
        (ruby-indent-line t)
        (end-of-line)))
    (setq
     ruby-block-highlight-toggle t
     ruby-block-delay 0.8)
    (add-hook 'ruby-ts-mode-hook 'ruby-electric-mode)))

(use-package yari
  :commands (yari)
  :ensure t)

;;;; Lisp

;;;; rainbow-delimiters
(use-package rainbow-delimiters
  :ensure t
  :commands rainbow-delimiters-mode
  :bind (("M-o m r" . rainbow-delimiters-mode)))

;;;; lisp-mode
(use-package lisp-mode
  :commands (lisp-mode)
  :init
  (progn
    (defun emacs-lisp-remove-elc-on-save ()
      "If you're saving an elisp file, likely the .elc is no longer valid."
      (make-local-variable 'after-save-hook)
      (add-hook 'after-save-hook
                (lambda ()
                  (when (and
                         buffer-file-name
                         (file-exists-p (concat buffer-file-name "c")))
                    (delete-file (concat buffer-file-name "c"))))))
    (add-hook 'emacs-lisp-mode-hook 'emacs-lisp-remove-elc-on-save)))

;;;; Julia
(use-package julia-mode
  :commands (julia-mode)
  :ensure t)

(use-package julia-shell
  :commands (julia-mode)
  :ensure t)

;;;; litable
(use-package litable
  :ensure t
  :commands (litable-mode)
  :init
  (progn
    (setq litable-list-file (expand-file-name ".litable-lists.el" boem-user-data-directory))))

;;;; CSS

;;;; css-mode
(use-package css-mode
  :ensure t
  :commands css-mode
  :mode ("\\.css\\'" . css-mode)
  :init
  (progn
    (setq css-indent-offset 2)))

;;;; scss-mode
(use-package scss-mode
  :ensure t
  :commands scss-mode
  :mode "\\.scss\\'"
  :init
  (progn
    (setq scss-compile-at-save nil))
  :config
  (progn
    (unbind-key "C-c C-c" scss-mode-map)))

;;;; sass-mode
(use-package sass-mode
  :ensure t
  :commands sass-mode
  :mode "\\.sass\\'")

;; Fires up a separate buffer where you can navigate JSON (buffer or region)
(use-package json-navigator
  :ensure t)

;;;; HTML

;;;; web-mode
(use-package web-mode
  :commands (web-mode)
  :ensure t
  :mode (("\\.phtml\\'" . web-mode) ("\\.erb\\'" . web-mode)
         ("\\.jsp\\'" . web-mode) ("\\.as[cp]x\\'" . web-mode)
         ("\\.blade\\.php\\'" . web-mode) ("\\.html\\'" . web-mode)
         ("\\.rhtml\\'" . web-mode) ("\\.mustache\\'" . web-mode)
         ("\\.hbs\\'" . web-mode)
         ("/\\(views\\|html\\|theme\\|templates\\)/.*\\.php\\'" . web-mode))
  :init
  (progn
    (setq
     web-mode-enable-auto-pairing nil
     web-mode-comment-style 2
     web-mode-indent-style 2
     web-mode-enable-whitespaces nil
     web-mode-enable-block-face t
     web-mode-enable-part-face t
     web-mode-enable-heredoc-fontification t
     web-mode-enable-comment-keywords t
     web-mode-enable-current-element-highlight t))
  :config
  (progn
    (bind-key "C-c ;" 'web-mode-comment-or-uncomment web-mode-map)
    (set-face-attribute 'web-mode-symbol-face nil :foreground "red")
    (unbind-key "C-c C-p" web-mode-map)
    (unbind-key "C-c C-n" web-mode-map)))

;;;; haml-mode
(use-package haml-mode
  :commands (haml-mode)
  :ensure t
  :mode (("\\.haml\\'" . haml-mode)))

;;;; impatient-mode
;;;; Use it for live html editing preview
(use-package impatient-mode
  :commands (impatient-mode)
  :ensure t)

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

;;; Elixir
(use-package elixir-ts-mode
  :ensure t
  :mode (("\\.ex\\'" . elixir-ts-mode)
         ("\\.exs\\'" . elixir-ts-mode)
         ("\\.heex\\'" . elixir-ts-mode))
  :init
  (eval-after-load "hideshow"
    '(add-to-list 'hs-special-modes-alist
                  `(elixir-ts-mode
                    ,(rx (or "def" "defp" "defmodule" "do" "{" "[" "if" "else" "unless" "describe" "setup" "test")) ; Block start
                    ,(rx (or "}" "]" "end"))                       ; Block end
                    ,(rx (or "#"))                        ; Comment start
                    )))
  (add-hook 'elixir-ts-mode-hook
            (lambda ()
              (hs-minor-mode)
              (add-hook 'before-save-hook #'eglot-format nil t))))

(use-package exunit
  :ensure t
  :config
  (add-hook 'elixir-mode-hook 'exunit-mode)
  (setq transient-default-level 5))

;;; Typescript
(use-package typescript-ts-mode
  ;; This is not needed for other modes but this one
  ;; needs this 'after' section
  :after tree-sitter
  :config
  ;; we choose this instead of tsx-mode so that eglot can
  ;; automatically figure out language for server see
  ;; https://github.com/joaotavora/eglot/issues/624 and
  ;; https://github.com/joaotavora/eglot#handling-quirky-servers
  (define-derived-mode typescriptreact-mode typescript-mode
    "TypeScript TSX")

  ;; use our derived mode for tsx files
  (add-to-list 'auto-mode-alist '("\\.tsx?\\'" . typescriptreact-mode))
  ;; by default, typescript-mode is mapped to the treesitter
  ;; typescript parser use our derived mode to map both .tsx AND .ts
  ;; -> typescriptreact-mode -> treesitter tsx
  (add-to-list 'tree-sitter-major-mode-language-alist
               '(typescriptreact-mode . tsx)))

(use-package eglot
  :hook (prog-mode . eglot-ensure))

;; Instructions. Manually download expert_darwin_arm64 from
;; GitHub and place it in ~/programs. If it does not exist
;; create symbolic link to it in /usr/local/bin or anywhere
;; else in the path.
(with-eval-after-load 'eglot
  (setf (alist-get '(elixir-mode elixir-ts-mode heex-ts-mode)
                   eglot-server-programs
                   nil nil #'equal)
        (if (and (fboundp 'w32-shell-dos-semantics)
                 (w32-shell-dos-semantics))
            '("expert_windows_amd64")
          (eglot-alternatives
           '("expert_linux_amd64" ("expert_darwin_arm64" "--stdio"))))))

;;;; yasnippet
(use-package yasnippet
  :ensure t
  :commands (yas-minor-mode)
  :diminish yas-minor-mode
  :init
  (progn
    (setq ;; Yasnippet
     ;; Dont print yasnippet messages
     yas-verbosity 0
     ;; Snippet directories
     boem-yas-snippets-dir (expand-file-name
                            "snippets" user-emacs-directory)
     ;; Disable yasnippet prompt by default
     ;; (using auto-complete to prompt)
     yas-prompt-functions '(yas-popup-isearch-prompt
                            yas-ido-prompt yas-completing-prompt yas-no-prompt))
    (add-to-list 'auto-mode-alist
                 (cons
                  (concat (regexp-quote boem-yas-snippets-dir) ".*\\'")
                  'snippet-mode))
    (add-to-list 'auto-mode-alist
                 (cons
                  (concat (regexp-quote (file-truename
                                         boem-yas-snippets-dir)) ".*\\'")
                  'snippet-mode))

    (bind-key "C-x i" 'yas-insert-snippet)
    (add-hook 'prog-mode-hook #'yas-minor-mode))
  :config
  (progn
    (bind-key "C-x i" 'yas-insert-snippet yas-minor-mode-map)
    (use-package popup
      :ensure t
      :commands yas-popup-isearch-prompt
      :config
      (progn
        ;; FIXME this should be niced up and contributed back.
        (defun yas-popup-isearch-prompt (prompt choices &optional display-fn)
          (let ((group-max-len 0)
                (key-max-len 0)
                (fmt "")
                (popup-items))

            (mapcar #'(lambda (choice)
                        (when (yas--template-p choice)
                          (setq group-max-len (max group-max-len
                                                   (+ (length (yas--template-group choice) )
                                                      (apply '+ (mapcar 'length (yas--template-group choice))))))
                          (setq key-max-len (max key-max-len (length (yas--template-key choice))))))
                    choices)

            (setq fmt (format "%s%%%d.%ds%s%%-%d.%ds│ %%s"
                              (if (> group-max-len 0 ) "" " ")
                              group-max-len group-max-len
                              (if (> group-max-len 0 ) " > " "")
                              key-max-len key-max-len))

            (setq popup-items
                  (mapcar
                   #'(lambda (choice)
                       (popup-make-item
                        (if (yas--template-p choice)
                            (format fmt
                                    (if (yas--template-group choice)
                                        (s-join "/" (yas--template-group choice))
                                      "")
                                    (if (yas--template-key choice)
                                        (yas--template-key choice)
                                      "")
                                    (if (yas--template-name choice)
                                        (yas--template-name choice)
                                      ""))
                          (format " %s" choice))
                        :value choice))
                   choices))

            (popup-menu*
             popup-items
             :prompt prompt
             :max-width 80
             :isearch t)))))

    (defun yas-remove-recompile-reload-all ()
      (interactive)
      (let ((default-directory boem-yas-snippets-dir) )
        (mapc (lambda (f)
                (delete-file f))
              (file-expand-wildcards "*.elc")))
      (f-files boem-yas-snippets-dir
               (lambda (file)
                 (and
                  (equal (f-no-ext (f-filename file)) ".yas-compiled-snippets")
                  (f-delete file)))
               t)
      ;; (yas-recompile-all)
      (yas-reload-all))

    (defun my-snippet-save-hook ()
      (when (and buffer-file-name
                 (eq major-mode 'snippet-mode))
        (yas-remove-recompile-reload-all)))

    (defun my-snippet-mode-hook ()
      (add-hook 'after-save-hook 'my-snippet-save-hook nil t))
    (add-hook 'snippet-mode-hook 'my-snippet-mode-hook)

    (defun dired-snippets-dir ()
      "Open dired in the yas snippets dir."
      (interactive)
      (dired (expand-file-name
              "snippets" user-emacs-directory)))

    (yas-reload-all))
  )

(use-package yasnippet-snippets
  :ensure t
  :after (yasnippet))

;;;; multiple-cursors
(use-package multiple-cursors
  :ensure t
  :commands (multiple-cursors-mode mc/edit-lines mc/mark-next-like-this
                                   mc/mark-next-word-like-this
                                   mc/mark-next-symbol-like-this
                                   mc/mark-previous-like-this
                                   mc/mark-previous-word-like-this
                                   mc/mark-previous-symbol-like-this
                                   mc/mark-more-like-this-extended
                                   mc/add-cursor-on-click
                                   mc/mark-all-like-this
                                   mc/mark-all-words-like-this
                                   mc/mark-all-symbols-like-this
                                   mc/mark-all-in-region
                                   mc/mark-all-like-this-in-defun
                                   mc/mark-all-words-like-this-in-defun
                                   mc/mark-all-symbols-like-this-in-defun
                                   mc/mark-all-like-this-dwim)
  :init
  (progn
    (setq mc/list-file (expand-file-name ".mc-lists.el" boem-user-data-directory)))
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)
         ("<mouse-2>" . mc/add-cursor-on-click)
         ("C-c c a" . mc/edit-beginnings-of-lines)
         ("C-c c e" . mc/edit-ends-of-lines))
  :config
  (progn
    (bind-key "C-s" 'phi-search mc/keymap)
    (bind-key "C-r" 'phi-search-backward mc/keymap)))

;;;; saveplace
(use-package saveplace
  :ensure t
  :config
  (progn
    (setq-default save-place t)))

;;;; rbenv
(use-package rbenv
  :ensure t
  :commands (rbenv-use rbenv-use-global global-rbenv-mode)
  :init
  (progn
    (setq rbenv-installation-dir "/usr/local/")
    (global-rbenv-mode)))

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

;;;; uniquify
(use-package uniquify
  :init
  (progn
    (setq
     uniquify-buffer-name-style 'post-forward
     uniquify-separator " • "
     uniquify-min-dir-content 3
     uniquify-after-kill-buffer-p t
     uniquify-ignore-buffers-re "^\\*")))

;;;; AsciiDoc
(use-package adoc-mode
  :ensure t
  :commands adoc-mode
  :mode ("\\.adoc\\'" . adoc-mode))

;;;; textile-mode
(use-package textile-mode
  :ensure t
  :commands textile-mode
  :mode ("\\.textile\\'" . textile-mode))

;;;; markdown-mode
(use-package markdown-mode
  :mode ("\\.md\\'" . markdown-ts-mode)
  :defer t)

;;;; org
(use-package org
  :commands (org-mode
             org-store-link
             org-agenda)
  :mode (("\\.org_archive\\'" . org-mode)
         ("\\.org\\'" . org-mode))
  :config
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.7))
  :init
  (progn
    (setq
     org-startup-folded t
     org-directory boem-user-org-directory
     org-src-fontify-natively t
     org-preview-latex-default-process 'dvisvgm
     org-insert-heading-respect-content t)
    ;; (setq org-refile-targets
    ;;       '((org-agenda-files :regexp . "Tasks")))
    (setq org-refile-targets
          '((org-agenda-files :level . 1)))

    (use-package org-habit
      :after (org)
      :config
      (progn
        (setq org-habit-graph-column 90)))

    (use-package org-agenda
      :commands (org-agenda)
      :bind ("C-c o a" . org-agenda)
      :config
      (setq
       org-agenda-files (directory-files boem-user-org-directory t "org$")
       org-agenda-include-diary nil
       org-agenda-time-leading-zero t
       org-agenda-time-grid (quote
                             ((daily today require-timed)
                              (0700 0900 1100 1300 1500 1700 1900 2100)
                              " ....." "-----------------"))
       org-agenda-prefix-format '((agenda . " %i %-20:c%?-12t% s") (todo . " %i %-20:c")
                                  (tags . " %i %-20:c") (search . " %i %-20:c"))
       org-agenda-custom-commands `(("A" "Daily agenda and top priority tasks"
                                     ,boem-custom-daily-agenda))))

    ;; After loading this package pinentry must be started
    ;; with (pinentry-start) and line
    ;; allow-emacs-pinentry
    ;; must be added to ~/.gnupg/gpg-agent.conf file
    (use-package pinentry
      :after (org)
      :ensure t)

    (use-package org-crypt
      :after (org)
      :config
      (progn
        (org-crypt-use-before-save-magic)
        (setq org-tags-exclude-from-inheritance (quote ("Encrypt")))
        (setq org-crypt-tag-matcher "Encrypt")
        ;; GPG key to use for encryption
        ;; Either the Key ID or set to nil to use symmetric encryption.
        (setq org-crypt-key nil)))

    (use-package org-capture
      :commands (org-capture)
      :bind ("C-c o c" . org-capture)
      :init
      (progn
        (setq org-capture-templates
              `(("t" "Todo" entry (file+headline ,(expand-file-name "todos.org" boem-user-org-directory) "Tasks")
                 "* TODO %?\n  %i\n  %a")
                ("j" "Journal" entry (file+datetree ,(expand-file-name "journal.org" boem-user-org-directory))
                 "* %?\Zapisano  %U\n  %i\n  %a")
                ("l" "Link" plain (file+headline ,(expand-file-name "za-citanje.org" boem-user-org-directory) "Nepročitani tabovi")
                 "  - %c %U")))))

    (use-package ob
      :after (org)
      :init
      (progn
        (org-babel-do-load-languages
         'org-babel-load-languages
         '((shell . t)
           (ditaa . t)
           (plantuml . t)
           (dot . t)
           (ruby . t)
           (emacs-lisp . t)
           (css . t)
           (sql . t)
           (js . t)
           (restclient . t)
           (graphql . t)))))

    (use-package ob-restclient
      :ensure t
      :after (ob))

    (use-package ob-graphql
      :ensure t
      :after (ob))

    (use-package org-superstar
      :after (org)
      :ensure t
      :init
      (add-hook 'org-mode-hook (lambda() (org-superstar-mode)))
      :config
      (setq org-superstar-special-todo-items t))
    )

  :config
  (progn
    (setq
     org-log-done 'time
     org-global-properties '(("Effort_ALL". "0 0:30 1:00 2:00 3:00 4:00 6:00 8:00"))
     org-columns-default-format "%50ITEM(Task) %6Effort{:} %10CLOCKSUM %SCHEDULED"
     org-tag-alist boem-org-tags
     org-ellipsis "…"
     org-clock-sound t
     org-enforce-todo-dependencies t
     org-enforce-todo-checkbox-dependencies t
     org-archive-location (concat (expand-file-name "archive.org" boem-user-org-directory) "::* From %s")
     org-imenu-depth 3)

    (setq
     org-todo-keywords
     '((sequence "TODO(t)" "|" "DONE(d)")
       (sequence "REPORT(r)" "BUG(b)" "FEATURE(e)" "|" "FIXED(x)")
       (sequence "DEFFERED(f)" "|" "CANCELED(c)"))
     org-todo-keyword-faces
      '(("TODO" :foreground "blue" :weight bold)
        ("BUG" :foreground "white" :weight bold)
        ("DONE" :foreground "red" :weight bold)
        ("DEFFERED" :foreground "orange" :weight bold)
        ("FEATURE" :foreground "magenta" :weight normal)
        ("CANCELLED" :foreground "red" :weight bold)
        ("FIXED" :foreground "yellow" :weight bold)
        ("REPORT" :foreground "yellow" :weight bold)))))

(use-package ox-md
  :commands (org-md-export-as-markdown
             org-md-export-to-markdown
             org-md-export-block))

;;;; imenu
(use-package imenu
  :init
  (progn
    (setq
     imenu-auto-rescan t)
    (use-package imenu-anywhere
      :ensure t
      :bind (("C-c i" . imenu-anywhere))
      :commands (imenu-anywhere))))

;;;; auto-revert-mode
(use-package autorevert
  :defer 1
  :init
  (progn
    (setq auto-revert-check-vc-info nil
          auto-revert-verbose nil)
    (setq auto-revert-mode-text " ♻"
          auto-revert-tail-mode-text " ♻~")
    (defun auto-revert-turn-on-maybe ()
      (unless (boem-current-buffer-remote-p)
        (auto-revert-mode)))
    (add-hook 'find-file-hook 'auto-revert-turn-on-maybe)))

;;;; csv-mode
(use-package csv-mode
  :ensure t
  :mode ("\\.[Cc][Ss][Vv]\\'" . csv-mode)
  :init
  (progn
    (setq csv-separators '("," ";" "|" " "))))

(use-package term
  :defer 1
  :bind (("C-c t" . boem-open-term))
  :init
  (progn
    (add-hook 'term-mode-hook #'(lambda ()
                                  (setq show-trailing-whitespace nil)))))

(use-package server
  :commands server-start-maybe
  :init
  (progn
    (add-hook 'after-init-hook
              'server-start-maybe))
  :config
  (progn
    (defun server-start-maybe ()
      (and (not (server-running-p))
         (server-start nil t)))))

;; Log commands to separate buffer
;; Keep this disabled since I do not need
;; it all the time.
(use-package mwe-log-commands
  :ensure t
  :disabled t)

(use-package which-key
  :commands (which-key)
  :init
  (add-hook 'after-init-hook 'which-key-mode)
  (setq which-key-idle-delay 0.5))

(use-package websocket
  :ensure t
  :commands (websocket-open))

(use-package gptel
  :ensure t
  :defer t
  :init
  (require 'gptel-integrations)
  :bind
  ("M-o w g" . gptel)
  ("M-o w m" . gptel-menu)
  ("M-o w t" . gptel-tools)
  ("M-o w h" . mcp-hub)
  )

(use-package mcp
  :ensure t
  :after gptel
  :custom (mcp-hub-servers
           '(("tidewave-elixir" . (:url "http://localhost:4000/tidewave/mcp"))
             ("tidewave-rails" . (:url "http://localhost:3000/tidewave/mcp"))))
  :config (require 'mcp-hub)
  )

(use-package boem-weather
  :vc (:url "https://gitlab.com/boskoivanisevic/boem-weather")
  :bind
  ("M-o w h" . boem-weather)
)

(provide 'init-packages)

;;; init-packages.el ends here
