;; init-packages.el --- Setup all packages
;;;; load packages

;;; Commentary:
;;

(require 'dired-x)
(if (< emacs-major-version 27)
    (require 'cl))

;;; Code:

(require 'em-term)

(use-package exec-path-from-shell
  :ensure t
  :commands (exec-path-from-shell-initialize)
  :init
  (progn
    (when (memq window-system '(mac ns x))
      (exec-path-from-shell-initialize))
    )
)

(use-package docker-cli
  :commands (docker-cli)
  :ensure t)

(use-package smart-mode-line
  :ensure t
  :config
  (progn
    (setq sml/no-confirm-load-theme t)
    (setq sml/theme 'dark)
    (sml/setup)))

(use-package neotree
  :commands (neotree)
  :ensure t)

(use-package all-the-icons
  :ensure t
  :commands (dired
             counsel-find-file
             find-file))

(use-package all-the-icons-ivy
  :after (ivy)
  :ensure t
  :config
  (if (display-graphic-p)
      (all-the-icons-ivy-setup)))

(use-package all-the-icons-dired
  :after (dired)
  :ensure t
  :init
  (progn
    (add-hook 'dired-mode-hook 'all-the-icons-dired-mode)))

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode)
  :custom
  (doom-modeline-height 25)
  (doom-modeline-bar-width 1)
  (doom-modeline-icon (display-graphic-p))
  (doom-modeline-major-mode-icon t)
  (doom-modeline-major-mode-color-icon t)
  (doom-modeline-buffer-file-name-style 'auto)
  (doom-modeline-buffer-state-icon t)
  (doom-modeline-buffer-modification-icon t)
  (doom-modeline-minor-modes nil)
  (doom-modeline-enable-word-count nil)
  (doom-modeline-buffer-encoding t)
  (doom-modeline-indent-info nil)
  (doom-modeline-checker-simple-format t)
  (doom-modeline-vcs-max-length 15)
  (doom-modeline-env-version t)
  )

;;; Keep this till I find how to get base16 light theme
;;; working in terminal
(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)

  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
  (doom-themes-treemacs-config)

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package base16-theme
  :ensure t
  :config
  (progn
    (if (display-graphic-p)
        (load-theme 'base16-gruvbox-light-medium t)
      (load-theme 'doom-gruvbox-light t))
    ))

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
  :commands (savehist-mode)
  :config
  (progn
    (setq
     ;; save searh entries
     savehist-additional-variables '(search ring regexp-search-ring)
     savehist-autosave-interval 60
     savehist-file (expand-file-name "savehist" boem-user-data-directory))
    (savehist-mode)))

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

;;;; diff-hl
(use-package diff-hl
  :ensure t
  :commands (diff-hl-mode
             turn-on-diff-hl-mode
             global-diff-hl-mode)
  :config
  (progn
    (use-package diff-hl-dired)
    (global-diff-hl-mode +1)
    (add-hook 'dired-mode-hook 'diff-hl-dired-mode)
    (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)))

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

;;;; gitignore-mode
(use-package gitignore-mode
  :commands (gitignore-mode)
  :ensure t
  :mode (("/\\.gitignore_global\\'" . gitignore-mode)
         ("/\\.gitignore\\'" . gitignore-mode)
         ("/\\.git/info/exclude\\'" . gitignore-mode)))

;;;; gitconfig-mode
(use-package gitconfig-mode
  :commands (gitconfig-mode)
  :ensure t
  :mode (("/\\.gitconfig\\'" . gitconfig-mode)
         ("/\\.git/config\\'" . gitconfig-mode)))

;;;; git-timemachine
(use-package git-timemachine
  :ensure t
  :commands (git-timemachine my-git-timemachine)
  :config
  (progn
    ;;;; Methods taken from
    ;;;; http://blog.binchen.org/posts/new-git-timemachine-ui-based-on-ivy-mode.html
    (defun my-git-timemachine-show-selected-revision ()
      "Show last (current) revision of file."
      (interactive)
      (let (collection)
        (setq collection
              (mapcar (lambda (rev)
                        ;; re-shape list for the ivy-read
                        (cons (concat (substring (nth 0 rev) 0 7) "|" (nth 5 rev) "|" (nth 6 rev)) rev))
                      (git-timemachine--revisions)))
        (ivy-read "commits:"
                  collection
                  :action (lambda (rev)
                            (git-timemachine-show-revision (cdr rev))))
        ))

    (defun my-git-timemachine ()
      "Open git snapshot with the selected version. Based on ivy-mode."
      (interactive)
      (unless (featurep 'git-timemachine)
        (require 'git-timemachine))
      (git-timemachine--start #'my-git-timemachine-show-selected-revision))
    )
  )

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
     magit-completing-read-function 'ivy-completing-read
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

(use-package ivy
  :commands (ivy-mode ivy-resume ivy-occur)
  :ensure t
  :config
  (progn
    (ivy-mode 1) ; globally at startup
    (setq ivy-use-virtual-buffers t)
    (setq ivy-height 20)
    (setq ivy-count-format "%d/%d ")
    ;; Example how Ivy regex builders can be mixed
    ;; (setq ivy-re-builders-alist
    ;;       '((ivy-switch-buffer . ivy--regex-plus)
    ;;         (t . ivy--regex-fuzzy)))
    ;; (setq ivy-re-builders-alist
    ;;       '((t . ivy--regex-fuzzy)))
    ;; Since fuzzy mathing is turned on we do not need
    ;; for ^ character to be inserted into input area.
    ;; It is only useful with default matcher.
    (setq ivy-initial-inputs-alist nil)
    (setq counsel-grep-base-command
          "rg -i --no-heading --line-number --color never '%s' %s")
    (set-face-background 'ivy-minibuffer-match-face-1 nil))
  )

(use-package counsel
  :commands (counsel-M-x counsel-git counsel-recentf)
  :ensure t
  :bind* ; load when pressed
  (("C-s"     . counsel-grep-or-swiper)
   ("C-c g"   . counsel-git)      ; search for files in git repo
   ("C-c c j" . counsel-git-grep) ; search for regexp in git repo
   ("C-c c k" . counsel-rg)       ; Use rg for regexp
   ("C-x l"   . counsel-locate)
   ("C-h f"   . counsel-describe-function)
   ("C-h v"   . counsel-describe-variable)
   ("C-c c l" . counsel-load-library)
   ("C-c c i" . counsel-info-lookup-symbol)
   ("C-c c u" . counsel-unicode-char)
   ("C-x C-f" . counsel-find-file)
   ("C-c c f" . counsel-find-library)
   ("C-x C-m" . counsel-M-x)
   ("C-c c r" . ivy-resume)
   ("C-c f"   . counsel-recentf)  ; search for recently edited
   ("C-c h i" . counsel-imenu)))

(use-package helm-dash
  :commands (helm-dash)
  :ensure t
  :config
  (progn
    (setq helm-dash-browser-func 'eww)
    (setq helm-dash-common-docsets '("Ruby_2" "Ruby_on_Rails_5" "NodeJS" "jQuery" "jQuery_UI" "Nginx" "MySQL" "JavaScript" "Elixir" "ElasticSearch" "Emacs_Lisp" "Jekyll" "Julia" "MongoDB" "PostgreSQL")))
  :bind (("C-h M-d" . helm-dash)
         ("C-h M-p" . helm-dash-at-point)))

(use-package company
  :commands (company-mode)
  :ensure t
  :config
  (progn
    (setq
     company-idle-delay 0.5
     company-tooltip-limit 10
     company-minimum-prefix-length 2
     company-tooltip-flip-when-above t)
    (global-company-mode))
  :diminish "co")

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
         ("/\\(views\\|html\\|theme\\|templates\\)/.*\\.php\\'" . web-mode)
         ("\\.l?eex\\'" . web-mode) ("\\.tsx" . web-mode))
  :init
  (progn
    (setq web-mode-enable-auto-pairing nil)
    (setq
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
          (add-to-list 'load-path "/usr/local/lib/erlang/lib/tools-3.3/emacs")
          (setq erlang-root-dir "/usr/local/lib/erlang")
          (setq exec-path (cons "/usr/local/lib/erlang/bin" exec-path))
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

;;; LSP
(use-package lsp-mode
  :bind ("C-c C-h" . lsp-describe-thing-at-point)
  :commands (lsp lsp-deferred)
  :ensure t
  :diminish lsp-mode
  :hook ((elixir-mode . lsp-deferred)
         (js2-mode . lsp-deferred)
         (typescript . lsp-deferred))
  :init
  (add-to-list 'exec-path "/Users/bosko/Code/elixir/elixir-ls/release")
  :config
  (progn
    (setq lsp-enable-file-watchers nil)))

(use-package lsp-ui
  :commands lsp-ui-mode
  :ensure t
  :config
  (setq lsp-ui-sideline-enable nil
        lsp-ui-doc-enable nil
        lsp-ui-flycheck-enable t
        lsp-ui-imenu-enable t
        lsp-ui-sideline-ignore-duplicate t))

(use-package company-lsp
  :commands company-lsp
  :ensure t
  :config
  (add-to-list 'company-backends 'company-lsp))

;;; Elixir
(use-package elixir-mode
  :commands (elixir-mode)
  :ensure t
  :mode (("\\.exs\\'" . elixir-mode)) ("\\.ex\\'" . elixir-mode))

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
    (bind-key "r" 'ivy-recentf ibuffer-mode-map)
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
  (progn
    (add-hook 'json-mode-hook
              #'(lambda ()
                  (setq-local js-indent-level 2)))))
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
  :init
  (progn
    (setq
     org-directory boem-user-org-directory
     org-src-fontify-natively t)
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
      (progn
        (setq org-agenda-time-grid (quote
                             ((daily today remove-match)
                              (0900 1100 1300 1500 1700)
                              "......" "----------------")))))

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
                 "* %?\Zapisano  %U\n  %i\n  %a")))))

    ;; (use-package org-journal
    ;;   :ensure t
    ;;   :commands (org-journal-new-entry)
    ;;   :init
    ;;   (progn
    ;;     (setq
    ;;      org-journal-dir
    ;;      (expand-file-name "journal/" boem-user-org-directory)
    ;;      org-journal-file-pattern "[0-9]\\{8\\}$")))

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
           (restclient . t)))))

    (use-package ob-restclient
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
     org-agenda-files (directory-files boem-user-org-directory t "org$")
     org-agenda-include-diary nil
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
  (progn
    (add-hook 'after-init-hook 'which-key-mode)))

(use-package websocket
  :ensure t
  :commands (websocket-open))

(provide 'init-packages)

;;; init-packages.el ends here
