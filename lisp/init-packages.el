;; init-packages.el --- Setup all packages
;;;; load packages

;;; Commentary:
;;

(require 'dired-x)
(if (< emacs-major-version 27)
    (require 'cl))

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

(require 'em-term)

(use-package exec-path-from-shell
  :ensure t
  :commands (exec-path-from-shell-initialize)
  :init
  (progn
    (when (memq window-system '(mac ns x))
      (exec-path-from-shell-initialize))))

(use-package docker-cli
  :commands (docker-cli)
  :ensure t)

(use-package dash-docs
  :ensure t
  :init
  (setq dash-docs-enable-debugging nil)
  :config
  (mapc #'(lambda (doc) (dash-docs-activate-docset doc)) (dash-docs-installed-docsets)))

;; Enable vertico
(use-package vertico
  :ensure t
  :init
  (vertico-mode)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  ;; (setq vertico-cycle t)
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
  ;;(setq resize-mini-windows t)

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
        completion-category-overrides '((file (styles . (partial-completion))) (eglot (styles orderless)))))

(use-package marginalia
  :ensure t
  :config
  (marginalia-mode))

;;;; embark
(use-package embark
  :ensure t
  :after (which-key)
  :init
  (setq embark-action-indicator
        (lambda (map _target)
          (which-key--show-keymap "Embark" map nil nil 'no-paging)
          #'which-key--hide-popup-ignore-command)
        embark-become-indicator embark-action-indicator)
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
  :demand t
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c h" . consult-history)
         ("C-c m" . consult-mode-command)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)
         ("C-x b" . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)
         ("C-x r x" . consult-register)
         ("C-x r b" . consult-bookmark)
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ("<help> a" . consult-apropos)            ;; orig. apropos-command
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g g" . consult-goto-line)
         ("M-g o" . consult-outline)       ;; "M-s o" is a good alternative.
         ("M-g m" . consult-mark)          ;; I recommend to bind Consult navigation
         ("M-g k" . consult-global-mark)   ;; commands under the "M-g" prefix.
         ("M-g i" . consult-imenu)         ;; or consult-imenu
         ;; M-s bindings (search-map)
         ("M-s f" . consult-find)          ;; or consult-locate, my-fdfind
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s m" . consult-multi-occur)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch)
         :map isearch-mode-map
         ("M-e" . consult-isearch)                 ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch)               ;; orig. isearch-edit-string
         ("M-s l" . consult-line))

  ;; The :init configuration is always executed (Not lazy!)
  :init

  ;; Configure register preview function.
  ;; This gives a consistent display for both `consult-register' and
  ;; the register preview when editing registers.
  (setq register-preview-delay 0
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  (if (executable-find "rg")
      (bind-key "C-c c k" 'consult-ripgrep)
    (bind-key "C-c c k" 'consult-git-grep))

  :config

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; (kbd "C-+")

  (setq consult-project-root-function #'vc-root-dir)
  (setq consult-config `((consult-ripgrep :preview-key ,(kbd "M-."))
                         (consult-grep :preview-key ,(kbd "M-."))
                         (consult-git-grep :preview-key ,(kbd "M-.")))))

;; Optionally add the `consult-flycheck' command.
(use-package consult-flycheck
  :bind (:map flycheck-command-map
              ("!" . consult-flycheck)))

(use-package consult-dash
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

(use-package modus-themes
  :ensure
  :init
  ;; Add all your customizations prior to loading the themes
  (setq modus-themes-slanted-constructs t
        modus-themes-hl-line '(accented intense)
        modus-themes-mode-line '(3d accented)
        modus-themes-prompts '(background intense bold)
        modus-themes-paren-match '(intense bold)
        modus-themes-bold-constructs nil)
  (modus-themes-load-themes)
  :config
  (if (eq nil (display-graphic-p))
      (modus-themes-load-operandi)
    (progn
      (modus-themes-load-vivendi)
      (set-face-attribute 'modus-themes-completion-selected nil :background "gray34")))
  :bind ("<f5>" . modus-themes-toggle))

(use-package neotree
  :commands (neotree)
  :ensure t
  :bind ("<f8>" . neotree-toggle))

(use-package all-the-icons
  :ensure t
  :if (display-graphic-p)
  :commands (dired
             consult-buffer
             find-file))

(use-package all-the-icons-dired
  :after (dired)
  :ensure t
  :if (display-graphic-p)
  :init
  (progn
    (add-hook 'dired-mode-hook 'all-the-icons-dired-mode)))

(use-package discover-my-major
  :ensure t
  :commands (discover-my-major discover-my-mode))
;; (use-package docker :ensure t
;;   :config
;;   (progn
;;     (setq docker-keymap-prefix "C-c C-d")
;;     (docker-global-mode)))
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
  (progn
    (setq vc-ignore-dir-regexp
          (format "\\(%s\\)\\|\\(%s\\)"
                  vc-ignore-dir-regexp
                  tramp-file-name-regexp))))

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
    (add-hook 'js2-mode-hook 'flycheck-turn-on-maybe)
    (add-hook 'js-mode-hook 'flycheck-turn-on-maybe)
    (add-hook 'json-mode-hook 'flycheck-turn-on-maybe)
    (add-hook 'ruby-mode-hook 'flycheck-turn-on-maybe)
    (add-hook 'php-mode-hook 'flycheck-turn-on-maybe)
    (add-hook 'scss-mode-hook 'flycheck-turn-on-maybe)
    (add-hook 'haskell-mode-hook 'flycheck-turn-on-maybe)
    (add-hook 'elixir-mode-hook 'flycheck-turn-on-maybe)

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
  :commands (magit-log magit-run-gitk magit-run-git-gui magit-blame
                       magit-status magit-git-repo-p magit-list-repos)
  :bind (("C-x g" . magit-status))
  :init
  (progn
    (setq
     magit-repo-dirs-depth 4
     magit-status-buffer-switch-function 'switch-to-buffer
     magit-save-some-buffers t
     ;; magit-completing-read-function 'ivy-completing-read
     ;; magit-diff-refine-hunk 'all
     magit-log-author-date-max-length 25
     magit-log-auto-more t
     magit-last-seen-setup-instructions "1.4.0")
    (use-package magit-blame
      :commands magit-blame-mode)
    (add-hook 'git-commit-mode-hook
              #'(lambda ()
                  (auto-fill-mode)
                  (git-commit-turn-on-flyspell))))
  :config
  (progn
    (require 'json)
    ;; (defadvice magit-status (around magit-fullscreen activate)
    ;;   (window-configuration-to-register :magit-fullscreen)
    ;;   ad-do-it
    ;;   (delete-other-windows))
    ;; (defun magit-quit-session ()
    ;;   "Restores the previous window configuration and kills the magit buffer"
    ;;   (interactive)
    ;;   ;; TODO maybe in some cases
    ;;   ;; (kill-buffer)
    ;;   (bury-buffer)
    ;;   (when (get-register :magit-fullscreen)
    ;;     (jump-to-register :magit-fullscreen)
    ;;     (set-register :magit-fullscreen nil)))
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
    (bind-key "W" 'magit-toggle-whitespace magit-status-mode-map)))

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
    ;; (setq undo-tree-auto-save-history t))

    (global-undo-tree-mode)

    (defadvice undo-tree-insert (around pretty activate)
      (ad-set-arg 0 (cond
                     ((equal ?| (ad-get-arg 0)) ?│)
                     ((equal ?\\ (ad-get-arg 0)) ?╲)
                     ((equal ?/ (ad-get-arg 0)) ?╱)
                     ((equal ?- (ad-get-arg 0)) ?─)
                     (t (ad-get-arg 0)))())
      ad-do-it))
  )

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
  :custom
  (corfu-cycle t)           ;; Enable cycling for `corfu-next/previous'
  (corfu-preselect 'prompt) ;; Always preselect the prompt

  :config
  (defun corfu-move-to-minibuffer ()
    (interactive)
    (let ((completion-extra-properties corfu--extra)
          completion-cycle-threshold completion-cycling)
      (apply #'consult-completion-in-region completion-in-region--data)))

  ;; Use TAB for cycling, default is `corfu-complete'.
  :bind
  (:map corfu-map
        ("TAB" . corfu-next)
        ([tab] . corfu-next)
        ("S-TAB" . corfu-previous)
        ([backtab] . corfu-previous)
        ("<escape>" . corfu-quit)
        ("M-m" . corfu-move-to-minibuffer))

  :init
  (use-package corfu-terminal
    :ensure t)

  (unless (display-graphic-p)
    (corfu-terminal-mode +1))

  (global-corfu-mode))

;;;; which-func
(use-package which-func
  :commands (which-func-mode)
  :init
  (progn
    (which-function-mode 1)))

;;;; Ruby

;;;; ruby-mode
(use-package ruby-mode
  :commands ruby-mode
  :init
  (progn
    (setq
     ;; Avoid default, ugly, Ruby indentation
     ruby-deep-indent-paren nil)
    (eval-after-load "hideshow"
      '(add-to-list 'hs-special-modes-alist
                    `(ruby-mode
                      ,(rx (or "def" "class" "module" "do" "{" "[" "if" "else" "unless")) ; Block start
                      ,(rx (or "}" "]" "end"))                       ; Block end
                      ,(rx (or "#" "=begin"))                        ; Comment start
                      ruby-forward-sexp nil)))
    (add-hook 'ruby-mode-hook (lambda() (hs-minor-mode))))
  :mode (("\\.rabl\\'" . ruby-mode)))

(use-package inf-ruby
  :ensure t
  :commands ruby-mode
  :config
  (progn
    (inf-ruby-minor-mode +1)
    (subword-mode +1))
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
    (add-hook 'ruby-mode-hook 'ruby-electric-mode)))

(use-package yari
  :commands (yari)
  :ensure t)

;;;; Lisp

;;;; rainbow-delimiters
(use-package rainbow-delimiters
  :ensure t
  :commands rainbow-delimiters-mode
  :bind (("M-o m r" . rainbow-delimiters-mode))
  :init
  (progn
    ;; (boem-hook-into-modes #'rainbow-delimiters-mode
    ;; '(clojure-mode-hook
    ;; emacs-lisp-mode-hook
    ;; haskell-mode-hook))
    ))

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

;;;; JavaScript

;;;; js
(use-package js
  :ensure t
  :commands (js-mode)
  :config
  (progn
    (boem-rename-modeline "js" js-mode "js")
    (font-lock-add-keywords
     'js-mode `(("\\(function *\\)("
                 (0 (progn (compose-region
                            (match-beginning 1)
                            (match-end 1) "ƒ") nil)))))
    (font-lock-add-keywords 'js-mode
                            '(("\\<\\(FIX\\|TODO\\|FIXME\\|HACK\\|REFACTOR\\):"
                               1 font-lock-warning-face t)))
    (add-hook 'js-mode-hook
              #'(lambda()
                  (setq-local js-indent-level 2)))))

;;;; js2-mode
(use-package js2-mode
  :ensure t
  :commands (js2-mode)
  :mode (("\\.js\\'" . js2-mode)
         ("\\.jsx\\'" . js2-mode))
  :init
  (progn
    (setq js2-strict-missing-semi-warning nil
          js2-basic-offset 2))
  :config
  (progn
    (use-package js2-imenu-extras
      :config
      (progn
        (js2-imenu-extras-setup)))

    (boem-rename-modeline "js2-mode" js2-mode "js2")

    (font-lock-add-keywords
     'js2-mode `(("\\(function *\\)("
                  (0 (progn (compose-region
                             (match-beginning 1)
                             (match-end 1) "ƒ") nil)))))
    (font-lock-add-keywords 'js2-mode
                            '(("\\<\\(FIX\\|TODO\\|FIXME\\|HACK\\|REFACTOR\\):"
                               1 font-lock-warning-face t)))))

;;;; typescript
(use-package typescript-mode
  :commands (typescript-mode)
  :ensure t
  :config
  (setq typescript-indent-level 2))

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
         ("/\\(views\\|html\\|theme\\|templates\\)/.*\\.php\\'" . web-mode)
         ("\\.html\\.heex\\'" . web-mode) ("\\.tsx" . web-mode))
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
     web-mode-enable-current-element-highlight t
     web-mode-engines-alist '(("elixir" . "\\.html\\.heex\\'"))))
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

;;;; yaml-mode
(use-package yaml-mode
  :ensure t
  :commands yaml-mode
  :mode ("\\.y[a]?ml\\'" . yaml-mode))

;;; Erlang
(cond
 ((string-equal system-type "darwin")
  (let ((asdf-erl (shell-command-to-string "asdf which erl")))
    (if (cl-search "command not found" asdf-erl)
        (progn
          (add-to-list 'load-path "/Users/bosko/.asdf/installs/erlang/25.0.4/lib/tools-3.5.3/emacs/")
          (setq erlang-root-dir "/Users/bosko/.asdf/installs/erlang/25.0.4/")
          (setq exec-path (cons "/Users/bosko/.asdf/installs/erlang/25.0.4/bin" exec-path))
          (require 'erlang-start))
      (progn
        (setq erlang-root-dir (substring asdf-erl 0 (cl-search "/bin/erl" asdf-erl)))
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
(use-package elixir-mode
  :commands (elixir-mode)
  :ensure t
  :mode (("\\.exs\\'" . elixir-mode)) ("\\.ex\\'" . elixir-mode)
  :init
  (eval-after-load "hideshow"
      '(add-to-list 'hs-special-modes-alist
                    `(elixir-mode
                      ,(rx (or "def" "defp" "defmodule" "do" "{" "[" "if" "else" "unless" "describe" "setup" "test")) ; Block start
                      ,(rx (or "}" "]" "end"))                       ; Block end
                      ,(rx (or "#"))                        ; Comment start
                      )))
  (add-hook 'elixir-mode-hook (lambda() (hs-minor-mode)))
  (add-hook 'elixir-mode-hook
            (lambda () (add-hook 'before-save-hook 'elixir-format))))

(use-package eglot
  :ensure t
  :after (elixir-mode)
  :config
  (add-to-list 'eglot-server-programs '(elixir-mode "~/Code/elixir/elixir-ls/release/language_server.sh"))
  :init
  (add-hook 'elixir-mode-hook 'eglot-ensure))

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
    ;; Switching to ibuffer puts the cursor on the most recent buffer
    (defadvice ibuffer (around ibuffer-point-to-most-recent activate)
      "Open ibuffer with cursor pointed to most recent buffer name"
      (let ((recent-buffer-name (buffer-name)))
        ad-do-it
        (ibuffer-update nil t)
        (unless (string= recent-buffer-name "*Ibuffer*")
          (ibuffer-jump-to-buffer recent-buffer-name)))))
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

    (require 'ibuf-ext)
    (define-ibuffer-filter filename2
        "Toggle current view to buffers with filename matching QUALIFIER."
      (:description "filename2"
                    :reader (read-from-minibuffer "Filter by filename (regexp): "))
      ;; (ibuffer-awhen (buffer-local-value 'buffer-file-name buf)
      (ibuffer-awhen (with-current-buffer buf
                       (or buffer-file-name
                           default-directory))
        (string-match qualifier it)))


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


    (defadvice ibuffer-invert-sorting (around ibuffer-point-to-same activate)
      "TODO"
      (let ((ibuf (get-buffer "*Ibuffer*")))
        (when ibuf
          (with-current-buffer ibuf
            (let ((selected-buffer (ibuffer-current-buffer)))
              ad-do-it
              (ibuffer-jump-to-buffer (buffer-name selected-buffer )))))))

    (defadvice ibuffer-toggle-sorting-mode (around ibuffer-point-to-same activate)
      "TODO"
      (let ((ibuf (get-buffer "*Ibuffer*")))
        (when ibuf
          (with-current-buffer ibuf
            (let ((selected-buffer (ibuffer-current-buffer)))
              ad-do-it
              (ibuffer-jump-to-buffer (buffer-name selected-buffer )))))))


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
                       (mode . ruby-mode)
                       (mode . elixir-mode)
                       (mode . js-mode)
                       (mode . js2-mode)
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
                       (mode . json-mode)
                       (mode . yaml-mode)
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

;;;; json-mode
(use-package json-mode
  :ensure t
  :commands json-mode
  :mode (("\\.json\\'" . json-mode)
         ("\\.ipynb\\'" . json-mode))
  :config
  (add-hook 'json-mode-hook
            #'(lambda ()
                (setq-local js-indent-level 2)
                (hs-minor-mode))))

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
  :ensure t
  :commands markdown-mode
  :mode (("\\.markdown\\'" . markdown-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.mdwn\\'" . markdown-mode)
         ("\\.mkd\\'" . markdown-mode)
         ("\\.mkdown\\'" . markdown-mode)
         ("\\.mdtext\\'" . markdown-mode))
  :init
  (progn
    (setq markdown-command "multimarkdown")
    (defun markdown-imenu-create-index ()
      (let* ((root '(nil . nil))
             cur-alist
             (cur-level 0)
             (pattern "^\\(\\(#+\\)[ \t]*\\(.+\\)\\|\\([^# \t\n=-].*\\)\n===+\\|\\([^# \t\n=-].*\\)\n---+\\)$")
             (empty-heading "-")
             (self-heading ".")
             hashes pos level heading)
        (save-excursion
          (goto-char (point-min))
          (while (re-search-forward pattern (point-max) t)
            (cond
             ((setq hashes (match-string-no-properties 2))
              (setq heading (match-string-no-properties 3)
                    pos (match-beginning 1)
                    level (length hashes)))
             ((setq heading (match-string-no-properties 4))
              (setq pos (match-beginning 4)
                    level 1))
             ((setq heading (match-string-no-properties 5))
              (setq pos (match-beginning 5)
                    level 2)))
            (let ((alist (list (cons heading pos))))
              (cond
               ((= cur-level level) ; new sibling
                (setcdr cur-alist alist)
                (setq cur-alist alist))
               ((< cur-level level) ; first child
                (dotimes (i (- level cur-level 1))
                  (setq alist (list (cons empty-heading alist))))
                (if cur-alist
                    (let* ((parent (car cur-alist))
                           (self-pos (cdr parent)))
                      (setcdr parent (cons
                                      (cons self-heading self-pos) alist)))
                  (setcdr root alist)) ;; primogenitor
                (setq cur-alist alist)
                (setq cur-level level))
               (t ;; new sibling of an ancestor
                (let ((sibling-alist (last (cdr root))))
                  (dotimes (i (1- level))
                    (setq sibling-alist (last (cdar sibling-alist))))
                  (setcdr sibling-alist alist)
                  (setq cur-alist alist))
                (setq cur-level level)))))
          (cdr root))))
    (add-hook 'markdown-mode-hook
              #'(lambda ()
                  (setq imenu-create-index-function
                        'markdown-imenu-create-index)))))

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
     org-preview-latex-default-process 'dvisvgm)
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
  :ensure t
  :init
  (add-hook 'after-init-hook 'which-key-mode)
  (setq which-key-idle-delay 0.5))

(use-package websocket
  :ensure t
  :commands (websocket-open))

(provide 'init-packages)

;;; init-packages.el ends here
