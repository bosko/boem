;;; internal-packages.el --- Internal Emacs packages -*- lexical-binding: t; -*-
;;
;; Author: Boško Ivanišević
;; URL: https://github.com/bosko/boem
;; Package-Requires: ((emacs "30.1"))
;; Keywords: config
;; SPDX-License-Identifier: GPL-3.0-or-later
;;

;;; Commentary:
;;  Enabling and configuring internal packages
;;

;;; Code:
(use-package expreg
  :ensure t
  :bind (("C-=" . expreg-expand)
         ("C--" . expreg-contract)))

(use-package dash
  :ensure t
  :defer t
  :init
  (progn
    (eval-after-load 'dash '(dash-enable-font-lock))))

(use-package dash-docs
  :ensure t
  :init
  (setq dash-docs-enable-debugging nil)
  (setq dash-docs-browser-func #'eww)
  :config
  (mapc #'(lambda (doc) (dash-docs-activate-docset doc)) (dash-docs-installed-docsets)))

(use-package csv-mode
  :ensure t
  :mode ("\\.[Cc][Ss][Vv]\\'" . csv-mode)
  :init
  (progn
    (setq csv-separators '("," ";" "|" " "))))

(use-package ghostel
  :ensure t)

(use-package ghostel-eshell
  :hook (eshell-mode . ghostel-eshell-visual-command-mode))

(use-package docker-cli
  :commands (docker-cli)
  :ensure t)

(use-package popper
  :ensure t
  :bind (("M-§"   . popper-toggle)
         ("C-§"   . popper-cycle)
         ("C-M-§" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "ghostel.*\\*$"
          "\\*emacs-eshell\\*"
          "^\\*eshell.*\\*$" eshell-mode
          help-mode))
  (popper-mode +1)
  (popper-echo-mode +1))

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

(use-package inf-ruby
  :ensure t
  :commands ruby-ts-mode
  :config
  (progn
    (inf-ruby-minor-mode +1))
  :init
  (progn
    (add-hook 'inf-ruby-mode-hook (lambda() (setq show-trailing-whitespace nil)))))

(use-package exunit
  :ensure t
  :config
  (add-hook 'elixir-mode-hook 'exunit-mode)
  (setq transient-default-level 5))

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

(use-package restclient
  :ensure t
  :commands (restclient-mode))

(use-package vlf
  :commands (vlf vlf-mode)
  :ensure t
  :config
  (progn
    (require 'json))
  )

(use-package wgrep
  :ensure t
  :defer t
  :bind (("C-x C-q" . boem-change-to-writable-mode)))

;;; AI tools

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
  :config
  ;; Mistral offers an OpenAI compatible API
  (gptel-make-openai "MistralLeChat"  ;Any name you want
    :host "api.mistral.ai"
    :endpoint "/v1/chat/completions"
    :protocol "https"
    :key 'gptel--get-api-key
    :models '("mistral-small-latest" "codestral-latest" "devstral-medium-latest"))
  )

(use-package mcp
  :ensure t
  :after gptel
  :custom (mcp-hub-servers
           '(("tidewave-elixir" . (:url "http://localhost:4000/tidewave/mcp"))
             ("tidewave-rails" . (:url "http://localhost:3000/tidewave/mcp"))))
  :config (require 'mcp-hub)
  )

(use-package agent-shell
  :ensure t
  :bind
  ("M-o w a" . agent-shell)
  :config
  (setq agent-shell-anthropic-authentication
        (agent-shell-anthropic-make-authentication
         :api-key (lambda () (auth-source-pick-first-password :host "anthropic")))
        agent-shell-openai-authentication
        (agent-shell-openai-make-authentication
         :api-key (lambda () (auth-source-pick-first-password :host "api.openai.com")))
        agent-shell-google-authentication
        (agent-shell-google-make-authentication :login t)
        agent-shell-session-strategy 'new
        agent-shell-prefer-viewport-interaction t
        agent-shell-mcp-servers
        '(((name . "tidewave-elixir")
           (type . "http")
           (headers . [])
           (url . "http://localhost:4000/tidewave/mcp"))
          ((name . "tidewave-rails")
           (type . "http")
           (headers . [])
           (url . "http://localhost:3000/tidewave/mcp"))))
  )

(provide 'external-packages)
