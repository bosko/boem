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
(use-package emacs
  :ensure nil
  :bind
  (("M-o a" . boem-kill-user-buffers)
   ("M-o b" . boem-switch-to-previous-buffer)
   ("M-o d" . duplicate-dwim)
   ("M-o e" . boem-pop-eshell-bottom)
   ("M-o g" . ghostel)
   ("M-o h" . windmove-left)
   ("M-o j" . windmove-down)
   ("M-o k" . windmove-up)
   ("M-o l" . windmove-right)
   ("M-o r" . boem-restclient)
   ("M-g r" . recentf)
   ("M-o w w" . boem-weather)
   ("M-s g" . grep)
   ("C-x C-m" . execute-extended-command)
   ("C-x C-b" . ibuffer)
   ("C-S-<return>" . boem-insert-line-above)
   ("S-<return>" . boem-insert-line)
   ("M-Z" . zap-up-to-char)
   ("C-g" . boem/keyboard-quit-dwim)
   ("C-x /" . boem-comment-uncomment)
   ("M-l" . scroll-down-line)
   ("M-k" . scroll-up-line)
   ("C-c o a" . org-agenda)
   ("C-c o c" . org-capture)
   ("C-c o i" . inf-ruby-console-auto)
   ;; Emacs-31
   ("C-x w t"  . window-layout-transpose)
   ;; Emacs-31
   ("C-x w r"  . window-layout-rotate-clockwise)
   ;; Emacs-31
   ("C-x w f h"  . window-layout-flip-leftright)
   ;; Emacs-31
   ("C-x w f v"  . window-layout-flip-topdown)
   ("C-x 5 l"  . select-frame-by-name)
   ("C-x 5 s"  . set-frame-name))
  :custom
  (ad-redefinition-action 'accept)
  (ansi-color-for-comint-mode t)
  (auto-save-default t)
  ;; We want auto-save, but no #file# cluterring, so everything goes
  ;; under our config cache/ (Directories are pre-created by
  ;; `boem/ensure-data-dirs'.)
  (auto-save-list-file-prefix (boem/data-path 'auto-saves-sessions))
  (auto-save-file-name-transforms `((".*" ,(boem/data-path 'auto-saves) t)))
  (bidi-display-reordering 'left-to-right)
  (bidi-inhibit-bpa t)
  (bidi-paragraph-direction 'left-to-right)
  (bookmark-file (boem/data-path 'bookmark-file))
  (buffer-file-coding-system 'utf-8)
  ; FIXME: is this even working?
  (shared-game-score-directory (boem/data-path 'shared-game-score-directory))
  (calendar-latitude 44.787197)
  (calendar-longitude 20.457273)
  (calendar-location-name "Београд, Србија")
  (calendar-day-name-array ["недеља" "понедељак" "уторак" "среда" "четвртак" "петак" "субота"])
  (calendar-day-abbrev-array ["не" "по" "ут" "ср" "че" "пе" "су"])
  (calendar-day-header-array ["не" "по" "ут" "ср" "че" "пе" "су"])
  (calendar-month-name-array ["Јануар" "Фебруар" "Март" "Април" "Мај" "Јун" "Јул"
                             "Август" "Септембар" "Октобар" "Новембар" "Децембар"])
  (calendar-week-start-day 1)
  (column-number-mode t)
  (cursor-in-non-selected-windows nil)
  (default-file-name-coding-system 'utf-8)
  (epa-armor t)
  (epa-pinentry-mode 'loopback)
  (fill-column 80)
  (line-number-mode t)
  (line-spacing nil)
  (locale-coding-system 'utf-8)
  (completion-ignore-case t)
  (completion-ignored-extensions
   '(".rbc" ".o" "~" ".bin" ".lbin" ".so" ".a" ".ln" ".blg"
     ".bbl" ".elc" ".lof" ".glo" ".idx" ".lot" ".svn/" ".hg/"
     ".git/" ".bzr/" "CVS/" "_darcs/" "_MTN/" ".fmt" ".tfm"
     ".class" ".fas" ".lib" ".mem" ".x86f" ".sparcf" ".dfsl"
     ".pfsl" ".d64fsl" ".p64fsl" ".lx64fsl" ".lx32fsl" ".dx64fsl"
     ".dx32fsl" ".fx64fsl" ".fx32fsl" ".sx64fsl" ".sx32fsl"
     ".wx64fsl" ".wx32fsl" ".fasl" ".ufsl" ".fsl" ".dxl" ".lo"
     ".la" ".gmo" ".mo" ".toc" ".aux" ".cp" ".fn" ".ky" ".pg"
     ".tp" ".vr" ".cps" ".fns" ".kys" ".pgs" ".tps" ".vrs" ".pyc"
     ".pyo" ".idx" ".lof" ".lot" ".glo" ".blg" ".bbl" ".cp" ".cps"
     ".fn" ".fns" ".ky" ".kys" ".pg" ".pgs" ".tp" ".tps" ".vr"
     ".vrs" ".sass-cache" ".min.js" "-min.js" ".min.css" "-min.css"
     ".hi" ".pyx" ".map"))
  (completions-detailed t)
  (delete-by-moving-to-trash t)
  (delete-pair-blink-delay 0)
  ; Emacs-31 for easy subsequent C-x C-x
  (delete-pair-push-mark t)
  (delete-selection-mode 1)
  (display-line-numbers-width 4)
  (display-line-numbers-widen t)
  ;; Emacs-31
  (display-fill-column-indicator-warning nil)
  (delete-selection-mode t)
  (echo-keystrokes 0.1)
  (enable-recursive minibuffers t)
  (ffap-machine-p-known 'reject)
  ;; find-dired results with human readable sizes
  (find-ls-option '("-exec ls -ldh {} +" . "-ldh"))
  (frame-resize-pixelwise t)
  ;; C-c RET on URLs open in default browser
  (global-goto-address-mode t)
  (global-hl-line-mode 1)
  (global-so-long-mode 1)
  (global-completion-preview-mode 1)
  ;; C-u C-c RET on URLs open in EWW
  (browse-url-secondary-browser-function 'eww-browse-url)
  (help-window-select t)
  (history-length 300)
  (indent-tabs-mode nil)
  (inhibit-splash-screen t)
  (inhibit-startup-message t)
  (initial-scratch-message "")
  ;; Emacs-31
  (ibuffer-human-readable-size t)
  ;; Emacs-31
  (ielm-history-file-name (boem/data-path 'ielm-history-file-name))
  (kill-do-not-save-duplicates t)
  ;; Emacs-31
  (kill-region-dwim 'emacs-word)
  ;; No lock files
  (create-lockfiles nil)
  ;; No backup files
  (make-backup-files nil)
  (multisession-directory (boem/data-path 'multisession-directory))
  (nsm-settings-file (boem/data-path 'nsm-settings-file))
  (next-line-add-newlines nil)
  (org-persist-directory (boem/data-path 'org-persist-dir))
  (package-user-dir boem-user-package-directory)
  (pixel-scroll-precision-mode t)
  (pixel-scroll-precision-use-momentum nil)
  (prefer-coding-system 'utf-8-unix)
  (project-list-file (boem/data-path 'project-list-file))
  ;; Excelent for mono repos with multiple langs, makes Eglot happy
  (project-vc-extra-root-markers '("Cargo.toml" "package.json" "go.mod" "*.asd"))
  (ring-bell-function 'ignore)
  (read-answer-short t)
  ;; 4MB
  (read-process-output-max (* 4 1024 1024))
  (reb-re-syntax 'string)
  ;; don't fontify during typing
  (redisplay-skip-fontification-on-input t)
  ;; default is 20
  (recentf-max-saved-items 300)
  (recentf-max-menu-items 15)
  (recentf-auto-cleanup (if (daemonp) 300 'never))
  (recentf-exclude (list "^/\\(?:ssh\\|su\\|sudo\\)?:"))
  (recentf-save-file (boem/data-path 'recentf-save-file))
  (register-use-preview t)
  (remote-file-name-inhibit-delete-by-moving-to-trash t)
  (remote-file-name-inhibit-auto-save t)
  (remote-file-name-inhibit-locks t)
  (remote-file-name-inhibit-auto-save-visited t)
  ;; 2MB
  (tramp-copy-size-limit (* 2 1024 1024))
  (tramp-use-scp-direct-remote-copying t)
  (tramp-verbose 1)
  (resize-mini-windows 'grow-only)
  ;; Keep syntax highlighting in current line.
  (set-face-foreground 'highlight nil)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-selection-coding-system 'utf-8)
  (set-language-environment 'utf-8)
  (set-default-coding-systems 'utf-8)
  (scroll-conservatively 10000)
  (scroll-up-aggressively 0.01)
  (scroll-down-aggressively 0.01)
  (scroll-step 0)
  (scroll-margin 1)
  (scroll-preserve-screen-position 1)
  (save-interprogram-paste-before-kill t)
  ;; t is default
  (savehist-save-minibuffer-history t)
  (savehist-additional-variables
   '(kill-ring                            ; clipboard
     register-alist                       ; macros
     mark-ring global-mark-ring           ; marks
     search-ring regexp-search-ring))     ; searches
  (savehist-file (boem/data-path 'savehist-file))
  (save-place-file (boem/data-path 'save-place-file))
  (save-place-limit 600)
  ;; C-u C-SPC jumps back to previous mark
  ;; and with this no need for C-u every time
  ;; except the first one - after that just
  ;; use C-SPC
  (set-mark-command-repeat-pop t)
  (show-trailing-whitespace t)
  ;; So vertical splits are preferred
  (split-width-threshold 170)
  (split-height-threshold nil)
  (shr-use-colors nil)
  (standard-indent 2)
  (switch-to-buffer-obey-display-actions t)
  (tab-always-indent 'complete)
  (tab-width 2)
  (transient-history-file (boem/data-path 'transient-history-file))
  (transient-levels-file (boem/data-path 'transient-levels-file))
  (transient-values-file (boem/data-path 'transient-values-file))
  (treesit-font-lock-level 4)
  ;; Emacs-31
  (treesit-auto-install-grammar 'always)
  ;; Emacs-31
  (treesit-enabled-modes t)
  (truncate-lines t)
  (undo-limit (* 13 160000))
  (undo-strong-limit (* 13 240000))
  (undo-outer-limit (* 13 24000000))
  (url-configuration-directory (boem/data-path 'url-configuration-directory))
  (use-dialog-box nil)
  (use-file-dialog nil)
  (use-package-compute-statistics t)
  (use-short-answers t)
  (view-read-only t)
  (visible-bell nil)
  ;; Emacs-31 auto updates C-h l usefull when teaching/debugging
  (view-lossage-auto-refresh t)
  (window-combination-resize t)
  (window-resize-pixelwise nil)
  ;; Treat clipboard input as UTF-8 string first; compound text next, etc.)
  (x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))
  (xref-search-program 'ripgrep)
  ;; Emacs-31
  (zone-all-frames t)
  ;; Emacs-31
  (zone-all-windows-in-frame t)
  (zone-programs '[zone-pgm-rat-race])
  ;; used by M-x grep
  (grep-command "rg -nS --no-heading ")
  ;; used if M-x rgrep uses find (default in grep-find-template)
  (grep-find-ignored-directories
   '("SCCS" "RCS" "CVS" "MCVS" ".src" ".svn" ".jj" ".git" ".hg" ".bzr" "_MTN" "_darcs" "{arch}" "node_modules" "build" "dist"))
  ; used by M-x rgrep (dropping find when using rg)
  (grep-find-template "rg <C> --null -nH -e <R> <D>")
  (pinentry-start)
  :config
  (add-hook 'dired-mode-hook #'dired-hide-details-mode)
  (add-hook 'prog-mode-hook #'hs-minor-mode)
  (add-hook 'ruby-mode-hook 'inf-ruby-minor-mode)
  (add-hook 'ruby-ts-mode-hook 'inf-ruby-minor-mode)

  ;; Make C-x 5 o repeatable
  (defvar-keymap frame-repeat-map
    :repeat t
    "o" #'other-frame
    "n" #'make-frame
    "d" #'delete-frame)
  (put 'other-frame 'repeat-map 'frame-repeat-map)

  ;; Source:
  ;; https://protesilaos.com/codelog/2024-11-28-basic-emacs-configuration/#h:1e468b2a-9bee-4571-8454-e3f5462d9321
  (defun boem/keyboard-quit-dwim ()
    "Do-What-I-Mean behaviour for a general `keyboard-quit'.

The generic `keyboard-quit' does not do the expected thing when
the minibuffer is open.  Whereas we want it to close the
minibuffer, even without explicitly focusing it.

The DWIM behaviour of this command is as follows:

- When the region is active, disable it.
- When a minibuffer is open, but not focused, close the minibuffer.
- When the Completions buffer is selected, close it.
- In every other case use the regular `keyboard-quit'."
    (interactive)
    (cond
     ((region-active-p)
      (keyboard-quit))
     ((derived-mode-p 'completion-list-mode)
      (delete-completion-window))
     ((> (minibuffer-depth) 0)
      (abort-recursive-edit))
     (t
      (keyboard-quit))))

  ;; Makes everything accept utf-8 as default, so buffers with tsx and so
  ;; won't ask for encoding (because undecided-unix) every single keystroke
  (modify-coding-system-alist 'file "" 'utf-8)

  (defun boem/load-theme ()
    "Loads `boem-gui-theme' or `boem-tui-theme' respectively if Emacs GUI or
in terminal is started"
    (interactive)
    (let ((loaded-theme
      (cond
       ((and (display-graphic-p))
        (load-theme (intern boem-gui-theme) t)
        boem-gui-theme)
       (boem-tui-theme
        (load-theme (intern boem-tui-theme) t)
        boem-tui-theme))))
      (if (string-search "modus" loaded-theme)
          (setq modus-themes-bold-constructs t
                modus-themes-prompts '(extrabold italic))
          (define-key global-map (kbd "<f5>") #'modus-themes-toggle))
      (message ">>> boem: loaded theme: %s" loaded-theme)))

  (boem/load-theme)

  ;; Configure preferred font if it exists
  (defun boem/configure-font ()
    (let* ((has-default-font
            (find-font (font-spec :family boem-preferred-font-name)))
           (size (nth (if (eq system-type 'darwin) 0 1)
                      boem-preferred-font-sizes)))
      (set-face-attribute 'default nil
                          :family (when has-default-font
                                    boem-preferred-font-name)
                          :height size)))

  ;; Load preferred font
  (boem/configure-font)

  ;; MacOS specific customizations
  (when (eq system-type 'darwin)
    (setq insert-directory-program "gls")
    (setq mac-command-modifier 'meta))

  ;; TRAMP specific HACKs
  ;; See https://coredumped.dev/2025/06/18/making-tramp-go-brrrr./
  (connection-local-set-profile-variables
   'remote-direct-async-process
   '((tramp-direct-async-process . t)))

  (connection-local-set-profiles
   '(:application tramp :protocol "scp")
   'remote-direct-async-process)

  (setq magit-tramp-pipe-stty-settings 'pty)

  (declare-function tramp-compile-disable-ssh-controlmaster-options "")
  (with-eval-after-load 'tramp
    (with-eval-after-load 'compile
      (remove-hook
       'compilation-mode-hook #'tramp-compile-disable-ssh-controlmaster-options)))

  ;; Disable VC on remote files - skips git/vc probing over TRAMP (faster navigation)
  (with-eval-after-load 'tramp
    (setq vc-ignore-dir-regexp
          (format "\\(%s\\)\\|\\(%s\\)"
                  vc-ignore-dir-regexp
                  tramp-file-name-regexp))
    (tramp-set-completion-function "ssh" '((tramp-parse-sconfig "~/.ssh/config")))
    (tramp-set-completion-function "scp" '((tramp-parse-sconfig "~/.ssh/config"))))

  (setopt tramp-persistency-file-name (boem/data-path 'tramp-persistency-file-name))

  (dolist (hook '(eshell-mode-hook
                  term-mode-hook
                  ghostel-mode-hook
                  eww-mode-hook
                  erc-mode-hook
                  shell-mode-hook
                  magit-diff-mode-hook
                  ibuffer-mode-hook
                  dired-mode-hook
                  occur-mode-hook
                  docker-cli-mode-hook
                  help-mode-hook
                  ;; From emacs-solo-container package
                  container-list-mode-hook))
    (add-hook hook '(lambda() (setq show-trailing-whitespace nil))))

  ;; A Protesilaos life savier HACK
  ;; Add option "d" to whenever using C-x s or C-x C-c, allowing a quick preview
  ;; of the diff (if you choose `d') of what you're asked to save.
  (add-to-list 'save-some-buffers-action-alist
               (list "d"
                     (lambda (buffer) (diff-buffer-with-file (buffer-file-name buffer)))
                     "show diff between the buffer and its file"))

  ;; Ibuffer filters
  (setq
   ibuffer-show-empty-filter-groups nil
   ibuffer-formats
   '(
     (
      mark
      (size 9 -1 :right)
      " "
      (mode 4 4 :right :elide)
      " "
      read-only
      modified
      " "
      (name 25 25 :left :elide)
      " "
      (vc-status-mini 1 1)
      " "
      filename-and-process)
     (mark " " (name 16 -1) " " filename))
   ibuffer-saved-filter-groups
   '(("default"
      ("org"     (or
                  (mode  . org-mode)
                  (name  . "^\\*Org Src")
                  (name  . "^\\*Org Agenda\\*$")))
      ("tramp"   (name   . "^\\*tramp.*"))
      ("emacs"   (or
                  (name  . "^\\*scratch\\*$")
                  (name  . "^\\*Messages\\*$")
                  (name  . "^\\*Warnings\\*$")
                  (name  . "^\\*Shell Command Output\\*$")
                  (name  . "^\\*Async-native-compile-log\\*$")))
      ("ediff"   (name   . "^\\*[Ee]diff.*"))
      ("scm" (or
              (mode . magit-status-mode)
              (mode . magit-log-mode)
              (mode . vc-annotate-mode)))
      ("dired"   (mode   . dired-mode))
      ("terminal" (or
                   (mode . term-mode)
                   (mode . shell-mode)
                   (mode . eshell-mode)))
      ("help"    (or
                  (name  . "^\\*Help\\*$")
                  (name  . "^\\*info\\*$")))
      ("news"    (name   . "^\\*Newsticker.*"))
      ("chat"    (or
                  (mode  . rcirc-mode)
                  (mode  . erc-mode)
                  (name  . "^\\*rcirc.*")
                  (name  . "^\\*ERC.*"))))))

  (add-hook 'ibuffer-mode-hook
            (lambda ()
              (ibuffer-switch-to-saved-filter-groups "default")
              (ibuffer-vc-set-filter-groups-by-vc-root)
              (unless (eq ibuffer-sorting-mode 'alphabetic)
                (ibuffer-do-sort-by-alphabetic))))

  ;; Colorize the '*Messages*' buffer
  (defun boem/messages-font-lock-setup ()
    (unless font-lock-defaults
      (setq-local font-lock-defaults '(nil nil nil nil nil)))
    (font-lock-add-keywords
     nil
     '(("^Loading .*"                      0 'shadow prepend)
       ("^Package .*"                      0 'shadow prepend)
       ("^line-move.*"                     0 'shadow prepend)
       ("^For information abou.*"          0 'shadow prepend)
       ("^Importing package-keyring.gpg.*" 0 'shadow prepend)
       ("^.*[Ee]rror:? .*"                 0 'compilation-error prepend)
       ("\\[.* times\\]"                   0 'font-lock-regexp-face prepend)
       ("done$"                            0 'font-lock-regexp-face prepend)
       ("^>>>.*"                           0 'font-lock-function-name-face prepend)))
    (font-lock-mode 1)
    (font-lock-flush)
    (font-lock-ensure))

  (add-hook 'messages-buffer-mode-hook #'boem/messages-font-lock-setup)

  (with-current-buffer (messages-buffer)
    (boem/messages-font-lock-setup))

  :init
  (when (>= emacs-major-version 31)
    ;; Emacs-31
    (tty-tip-mode nil))
  (tooltip-mode nil)

  (select-frame-set-input-focus (selected-frame))
  (blink-cursor-mode 0)
  (recentf-mode 1)
  (repeat-mode 1)
  (savehist-mode 1)
  (save-place-mode 1)
  (winner-mode)
  (xterm-mouse-mode 1)
  ;; allows us to type a new path without having to delete the current one
  (file-name-shadow-mode 1)

  (with-current-buffer (get-buffer-create "*scratch*")
    (insert (format ";;
;; ██████▓▒░    █████▓▒░   ███████▓▒░  ██▓▒░   ██▓▒░
;; ██▓▒░██▓▒░  ██▓▒░██▓▒░  ██▓▒░       ███▓▒░ ███▓▒░
;; ██▓▒░██▓▒░  ██▓▒░██▓▒░  ██▓▒░       ████▓▒████▓▒░
;; ██████▓▒░   ██▓▒░██▓▒░  ██████▓▒░   ██▓███▓██▓▒░
;; ██▓▒░██▓▒░  ██▓▒░██▓▒░  ██▓▒░       ██▓▒██▓▒██▓▒░
;; ██▓▒░██▓▒░  ██▓▒░██▓▒░  ██▓▒░       ██▓▒░░░ ██▓▒░
;; ██████▓▒░    █████▓▒░   ███████▓▒░  ██▓▒░   ██▓▒░
;;
;;   Loading time : %s
;;   Packages     : %s
;;
"
                    (emacs-init-time)
                    (number-to-string (length package-activated-list))))
    (not-modified))

  (add-hook 'emacs-startup-hook
          (lambda ()
            (when-let ((win (get-buffer-window "*scratch*")))
              (select-window win)
              (goto-char (point-max)))))

  (message ">>> boem: init time %s" (emacs-init-time)))

;; Configuration for packages below are mostly taken from emacs-solo
;; project with some adjustments to my needs.

;; =========================

(use-package auth-source
  :ensure nil
  :defer t
  :config
  (setq epg-pinentry-mode 'loopback
        user-full-name "User Name and Surnames"
        user-mail-address "user@mail.com")

  ;; Use `pass` as an auth-source
  (when (file-exists-p "~/.password-store")
    (auth-source-pass-enable)))


(use-package autorevert
  :ensure nil
  :hook (emacs-startup . global-auto-revert-mode)
  :custom
  (auto-revert-remote-files nil)   ;; t makes tramp slow
  (auto-revert-verbose t)
  (auto-revert-avoid-polling t)
  (global-auto-revert-non-file-buffers t)
  (auto-revert-check-vc-info nil)
  (auto-revert-mode-text " ♻")
  (auto-revert-tail-mode-text " ♻~"))

;; =========================

(use-package conf-mode
  :ensure nil
  :mode ("\\.env\\..*\\'" "\\.env\\'")
  :init
  (add-to-list 'auto-mode-alist '("\\.env\\'" . conf-mode)))

;; =========================

(use-package compile
  :ensure nil
  :custom
  (compilation-always-kill t)
  (compilation-scroll-output t)
  (ansi-color-for-compilation-mode t))

;; =========================

(use-package window
  :ensure nil
  :custom
  (display-buffer-alist
   '(("\\*\\(Backtrace\\|Warnings\\|Compile-Log\\|Messages\\|Bookmark List\\|Occur\\|eldoc\\)\\*"
      (display-buffer-in-side-window)
      (window-height . 0.25)
      (side . bottom)
      (slot . 0))
     ("\\*\\([Hh]elp\\)\\*"
      (display-buffer-in-side-window)
      (window-width . 75)
      (side . right)
      (slot . 0))
     ("\\*\\(Ibuffer\\)\\*"
      (display-buffer-in-side-window)
      (window-width . 100)
      (side . right)
      (slot . 1))
     ("\\*\\(claude:\\|opencode:\\).*\\*"
      (display-buffer-in-side-window)
      (window-width . 100)
      (side . right)
      (slot . 1))
     ("\\*\\(Flymake diagnostics\\)"
      (display-buffer-in-side-window)
      (window-height . 0.25)
      (side . bottom)
      (slot . 2))
     ("\\*\\(grep\\|xref\\|find\\)\\*"
      (display-buffer-in-side-window)
      (window-height . 0.25)
      (side . bottom)
      (slot . 1))
     ("\\*inferior.*"
      (display-buffer-in-side-window)
      (window-height . 0.5)
      (side . bottom)
      (slot . 1))
     ("\\*\\(M3U Playlist\\)"
      (display-buffer-in-side-window)
      (window-height . 0.25)
      (side . bottom)
      (slot . 3)))))

;; =========================

;;  Customizations:
;;  C-<tab> and C-<backtab> cycles tabs inside a group (if inside a group)
;;  M-<tab>                 cycles between groups
(use-package tab-bar
  :ensure nil
  :defer t
  :bind
  (("C-x t <left>"  . tab-bar-history-back)
   ("C-x t <right>" . tab-bar-history-forward)
   ("C-x t P"       . #'boem/tab-group-from-project)
   ("C-x t g"       . #'boem/tab-switch-to-group)
   ("C-x t RET"     . #'boem/tab-select-by-number))
  :custom
  (tab-bar-new-tab-choice "*scratch*")
  (tab-bar-close-button-show nil)
  (tab-bar-new-button-show nil)
  (tab-bar-tab-hints t)
  (tab-bar-auto-width nil)
  (tab-bar-separator "")
  (tab-bar-format '(tab-bar-format-tabs-groups
                    tab-bar-separator
                    tab-bar-format-align-right
                    tab-bar-format-global))
  :init
  ;;; --- OPTIONAL INTERNAL FN OVERRIDES TO DECORATE NAMES
  (defun tab-bar-tab-name-format-hints (name tab i)
    (let ((open-glyph  (if (char-displayable-p ?⌞) "⌞" "["))
          (close-glyph (if (char-displayable-p ?⌝) "⌝" "]")))
      (if tab-bar-tab-hints
          (if (eq (car tab) 'current-tab)
              (concat (format "  %s%d%s  " open-glyph i close-glyph) "")
            (concat (format "   %d   " i) ""))
        name)))

  ;;; --- MAKE DISABLED GROUP NOT BE RENDERED
  (defun tab-bar-tab-group-format-default (tab _i &optional current-p)
    (if current-p
        (propertize
         (concat
          (if (char-displayable-p ?) "   " " [p] ")
          (funcall tab-bar-tab-group-function tab))
         'face 'tab-bar-tab-group-current)
      ""))

  (defun boem/tab-bar-toggle-time ()
    "Enable `display-time-mode' when `tab-bar-mode' is on, disable it otherwise."
    (setq display-time-format "%a. %d %b %H:%M")
    (if tab-bar-mode
        (display-time-mode 1)
      (display-time-mode -1)))

  (add-hook 'tab-bar-mode-hook #'boem/tab-bar-toggle-time)

  (defun boem/tab-select-by-number ()
    "Switch to a tab by its hint number."
    (interactive)
    (let ((num (read-number "Tab number: ")))
      (tab-bar-select-tab num)))

  ;;; --- UTILITIES FUNCTIONS
  (defun boem/tab-group-from-project ()
    "Call `tab-group` with the current project name as the group."
    (interactive)
    (when-let* ((proj (project-current))
                (name (file-name-nondirectory
                       (directory-file-name (project-root proj)))))
      (tab-group (format "%s" name))))

  (defun boem/tab-switch-to-group ()
    "Prompt for a tab group and switch to its first tab.
Uses position instead of index field."
    (interactive)
    (let* ((tabs (funcall tab-bar-tabs-function)))
      (let* ((groups (delete-dups (mapcar (lambda (tab)
                                            (funcall tab-bar-tab-group-function tab))
                                          tabs)))
             (group (completing-read "Switch to group: " groups nil t)))
        (let ((i 1) (found nil))
          (dolist (tab tabs)
            (let ((tab-group (funcall tab-bar-tab-group-function tab)))
              (when (and (not found)
                         (string= tab-group group))
                (setq found t)
                (tab-bar-select-tab i)))
            (setq i (1+ i)))))))

  ;;; --- TURNS ON BY DEFAULT
  (tab-bar-mode 1)
  (tab-bar-history-mode 1))

;; =========================

(use-package erc
  :ensure nil
  :defer t
  :custom
  (erc-join-buffer 'window)
  (erc-hide-list '("JOIN" "PART" "QUIT"))
  (erc-timestamp-format "[%H:%M]")
  (erc-autojoin-channels-alist '((".*\\.libera\\.chat" "#emacs" "#systemcrafters")))
  (erc-server-reconnect-attempts 10)
  (erc-server-reconnect-timeout 3)
  (erc-fill-function 'erc-fill-wrap)
  (erc-log-channels-directory (boem--cache-path 'erc-log-channels-directory))
  ;; Emacs-31 and or needs https://debbugs.gnu.org/cgi/bugreport.cgi?bug=79665 patch
  (erc-log-insert-log-on-open 'erc-log-new-target-buffer-p)
  (erc-save-buffer-on-part t)
  (erc-save-queries-on-quit t)
  (erc-log-write-after-send t)
  (erc-log-write-after-insert t)
  (erc-spelling-dictionaries '(("Libera.Chat" "en_US")))
  :config
  (defun boem/erc-get-color-for-nick (nick)
    "Return a Catppuccin Mocha Like color string for NICK based on its hash."
    (let* ((colors '("#f38ba8" "#a6e3a1" "#f9e2af" "#89b4fa"
                     "#cba6f7" "#fab387" "#b4befe" "#eba0ac"
                     "#f5c2e7"))
           (hash (mod (abs (sxhash nick)) (length colors))))
      (nth hash colors)))

  (defun boem/erc-colorize-nick ()
    "Colorize nicknames in ERC buffer."
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "\\(<\\)\\([^ >]+\\)\\(>\\)" nil t)
        (let* ((nick (match-string 2))
               (color (boem/erc-get-color-for-nick nick)))
          (put-text-property (match-beginning 2) (match-end 2)
                             'face `(:foreground ,color :weight bold))))))
  (add-hook 'erc-insert-modify-hook #'boem/erc-colorize-nick)

  (add-to-list 'erc-modules 'log)
  (erc-spelling-mode 1)
  :init
  (with-eval-after-load 'erc
    ;; Emacs-31 (no more dependency between scrolltobottom and erc-fill-wrap THX!!!)
    (when (< emacs-major-version 31)
      (add-to-list 'erc-modules 'scrolltobottom)))

  (setopt erc-sasl-mechanism 'external)

  (defun erc-liberachat ()
    (interactive)

    (with-eval-after-load 'erc
      (add-to-list 'erc-modules 'sasl))

    (let ((buf (erc-tls :server "irc.libera.chat"
                        :port 6697
                        :user "Lionyx"
                        :password ""
                        :client-certificate
                        (list
                         (expand-file-name "cert.pem" user-emacs-directory)
                         (expand-file-name "cert.pem" user-emacs-directory)))))
      (when (bufferp buf)
        (pop-to-buffer buf)))))

;; =========================

(use-package dired
  :ensure nil
  :custom
  (dired-auto-revert-buffer t)
  (dired-dwim-target t)
  (dired-guess-shell-alist-user
   `(("\\.\\(png\\|jpe?g\\|tiff\\)" ,(if (eq system-type 'darwin) "open" "xdg-open"))
     ("\\.\\(mp[34]\\|m4a\\|ogg\\|flac\\|webm\\|mkv\\)" "mpv")
     (".*" ,(if (eq system-type 'darwin) "open" "xdg-open"))))
  (dired-kill-when-opening-new-dired-buffer t)
  (dired-listing-switches "-alh --group-directories-first")
  ;; with dired-omit-mode (C-x M-o)
  (dired-omit-files "^\\.")
  ;; Emacs-31
  (image-dired-dir (boem/data-path 'image-dired-dir))
  :init
  (add-hook 'dired-mode-hook
            (lambda ()
              ;; Turning this ON also sets the C-x M-o binding.
              (dired-omit-mode 1)
              ;; This makes C-c RET C-a add attachments.
              (turn-on-gnus-dired-mode)))

  (defun boem/dired-rsync-copy (dest)
    "Copy marked files in Dired to DEST using rsync in an async shell buffer."
    (interactive
     (list (expand-file-name (read-file-name "rsync to: "
                                             (dired-dwim-target-directory)))))
    (let* ((files (dired-get-marked-files nil current-prefix-arg))
           (dest-original dest)
           (dest-rsync
            (if (file-remote-p dest)
                (let* ((vec (tramp-dissect-file-name dest))
                       (user (tramp-file-name-user vec))
                       (host (tramp-file-name-host vec))
                       (path (tramp-file-name-localname vec)))
                  (concat (if user (concat user "@") "")
                          host
                          ":"
                          path))
              dest))
           (files-rsync
            (mapcar
             (lambda (f)
               (if (file-remote-p f)
                   (let ((vec (tramp-dissect-file-name f)))
                     (let ((user (tramp-file-name-user vec))
                           (host (tramp-file-name-host vec))
                           (path (tramp-file-name-localname vec)))
                       (concat (if user (concat user "@") "")
                               host
                               ":"
                               path)))
                 f))
             files))
           (command (append '("rsync" "-hPur") files-rsync (list dest-rsync)))
           (buffer (get-buffer-create "*rsync*")))

      (message ">>> boem: rsync original dest %s" dest-original)
      (message ">>> boem: rsync converted dest %s" dest-rsync)
      (message ">>> boem: rsync source files %s" files-rsync)
      (message ">>> boem: rsync command %s" (string-join command " "))

      (with-current-buffer buffer
        (erase-buffer)
        (insert "Running rsync...\n"))

      (defun rsync-process-filter (proc string)
        (with-current-buffer (process-buffer proc)
          (goto-char (point-max))
          (insert string)
          (goto-char (point-max))
          (while (re-search-backward "\r" nil t)
            (replace-match "\n" nil nil))))

      (make-process
       :name "dired-rsync"
       :buffer buffer
       :command command
       :filter #'rsync-process-filter
       :sentinel
       (lambda (_proc event)
         (when (string-match-p "finished" event)
           (with-current-buffer buffer
             (goto-char (point-max))
             (insert "\n* rsync done *\n"))
           (dired-revert)))
       :stderr buffer)

      (display-buffer buffer)
      (message ">>> boem: rsync started...")))

  (defun boem/dired-run-async-on-marked-files (command)
    "Run COMMAND asynchronously on marked files in Dired.
Ex: mpv file1 file2 file3 file4..."
    (interactive "sCommand: ")
    (let ((files (dired-get-marked-files)))
      (start-process-shell-command
       command
       nil
       (format "%s %s" command (mapconcat 'shell-quote-argument files " ")))))


  (eval-after-load 'dired
    '(progn
       (define-key dired-mode-map (kbd "#") 'boem/dired-run-async-on-marked-files)
       ;; A better "BACK" keybiding
       (define-key dired-mode-map (kbd "b") 'dired-up-directory))))

;; =========================

(use-package wdired
  :ensure nil
  :commands (wdired-change-to-wdired-mode)
  :config
  (setq wdired-allow-to-change-permissions t)
  (setq wdired-create-parent-directories t))

;; =========================

(use-package doc-view
  :ensure nil
  :custom
  (doc-view-resolution 200))

;; =========================

(use-package eshell
  :ensure nil
  :defer t
  :config
  (setq eshell-history-size 100000
        eshell-hist-ignoredups t))

;; =========================

(use-package isearch
  :ensure nil
  :config
  (setq isearch-lazy-count t
        lazy-count-prefix-format "(%s/%s) "
        lazy-count-suffix-format nil
        search-whitespace-regexp ".*?"
        isearch-allow-scroll t)

  (defun isearch-copy-selected-word ()
    "Copy the current `isearch` selection to the kill ring."
    (interactive)
    (when isearch-other-end
      (let ((selection (buffer-substring-no-properties isearch-other-end (point))))
        (kill-new selection)
        (isearch-exit))))

  ;; Bind `M-w` in isearch to copy the selected word, so M-s M-. M-w
  ;; does a great job of 'copying the current word under cursor'.
  (define-key isearch-mode-map (kbd "M-w") 'isearch-copy-selected-word))

;; =========================

(use-package smerge-mode
  :ensure nil
  :bind (:map smerge-mode-map
              ("C-c C-s C-u" . smerge-keep-upper)
              ("C-c C-s C-l" . smerge-keep-lower)
              ("C-c C-s C-n" . smerge-next)
              ("C-c C-s C-p" . smerge-prev)))

;; =========================

(use-package diff-mode
  :ensure nil
  :defer t
  :bind (:map diff-mode-map
              ("M-o" . other-window))
  :config
  (setq diff-default-read-only t
        diff-advance-after-apply-hunk t
        diff-update-on-the-fly t
        diff-font-lock-syntax 'hunk-also
        diff-font-lock-prettify nil))

;; =========================

(use-package ediff
  :ensure nil
  :commands (ediff-buffers ediff-files ediff-buffers3 ediff-files3)
  :init
  (setq ediff-split-window-function 'split-window-horizontally
        ediff-window-setup-function 'ediff-setup-windows-plain)
  :config
  (setq ediff-keep-variants nil
        ediff-make-buffers-readonly-at-startup nil
        ediff-show-clashes-only t))

;; =========================

(use-package eldoc
  :ensure nil
  :custom
  (eldoc-help-at-pt t) ;; EMACS-31
  (eldoc-echo-area-use-multiline-p nil)
  (eldoc-echo-area-prefer-doc-buffer t)
  (eldoc-documentation-strategy 'eldoc-documentation-compose)
  :init
  (global-eldoc-mode))

;; =========================

(use-package eglot
  :ensure nil
  :custom
  (eglot-autoshutdown t)
  (eglot-events-buffer-config '(:size 0 :format full))
  (eglot-prefer-plaintext nil)
  (jsonrpc-event-hook nil)
  ;; (eglot-code-action-indications nil)
  ;; EMACS-31 -- use the 'experimental' internal markdown-ts-mode to render docs
  (eglot-documentation-renderer 'markdown-ts-view-mode)
  :init
  (fset #'jsonrpc--log-event #'ignore)

  (setq-default eglot-workspace-configuration (quote
                                               (:gopls (:hints (:parameterNames t)))))

  (defun boem/eglot-setup ()
    "Setup eglot mode with specific exclusions."
    (unless (memq major-mode '(emacs-lisp-mode lisp-mode))
      (eglot-ensure)))

  (add-hook 'prog-mode-hook #'boem/eglot-setup)

  (with-eval-after-load 'eglot
    (add-to-list
     'eglot-server-programs
    '((elixir-mode elixir-ts-mode heex-ts-mode) "expert_darwin_arm64" "--stdio")))

  (with-eval-after-load 'eglot
    (add-to-list
     'eglot-server-programs
     '((ruby-mode ruby-ts-mode) "ruby-lsp")))

  (with-eval-after-load 'eglot
    (add-to-list
     'eglot-server-programs
     '((tsx-ts-mode typescript-ts-mode js-mode js-jsx-mode js-ts-mode)
       . ("rass"
          "--"
          "typescript-language-server" "--stdio"
          "--"
          "eslint-lsp" "--stdio"
          "--"
          "tailwindcss-language-server" "--stdio"))))

  :bind (:map
         eglot-mode-map
         ("C-c l a" . eglot-code-actions)
         ("C-c l o" . eglot-code-action-organize-imports)
         ("C-c l r" . eglot-rename)
         ("C-c l i" . eglot-inlay-hints-mode)
         ("C-c l f" . eglot-format)))

;; =========================

(use-package flymake
  :ensure nil
  :defer t
  :hook (prog-mode-hook . flymake-mode)
  :bind (:map flymake-mode-map
              ("C-c ! n" . flymake-goto-next-error)
              ("C-c ! p" . flymake-goto-prev-error)
              ("C-c ! l" . flymake-show-buffer-diagnostics)
              ("C-c ! t" . toggle-flymake-diagnostics-at-eol))
  :custom
  (flymake-show-diagnostics-at-end-of-line 'short)
  (flymake-indicator-type 'margins)
  (flymake-margin-indicators-string
   ;; Alternatives: », E, W, i, !, ?, ⚠️)
   `((error "!" compilation-error)
     (warning "?" compilation-warning)
     (note "i" compilation-info)))
  :config
  ;; Define the toggle function
  (defun toggle-flymake-diagnostics-at-eol ()
    "Toggle the display of Flymake diagnostics at the end of the line
and restart Flymake to apply the changes."
    (interactive)
    (setq flymake-show-diagnostics-at-end-of-line
          (not flymake-show-diagnostics-at-end-of-line))
    (flymake-mode -1) ;; Disable Flymake
    (flymake-mode 1)  ;; Re-enable Flymake
    (message ">>> boem: Flymake diagnostics at end of line %s"
             (if flymake-show-diagnostics-at-end-of-line
                 "Enabled" "Disabled"))))

;; =========================

(use-package flyspell
  :ensure nil
  :defer t
  :config
  (ispell-set-spellchecker-params)
  :hook
  ((text-mode-hook . flyspell-mode)
   (prog-mode-hook . flyspell-prog-mode)))

;; =========================

(use-package whitespace
  :ensure nil
  :defer t
  :bind (("C-c w" . whitespace-cleanup))
  :hook (before-save-hook . whitespace-cleanup)
  :init
  (defun boem/toggle-whitespace-cleanup-on-save ()
    "Toggle whitespace-cleanup on save."
    (interactive)
    (if (memq #'whitespace-cleanup before-save-hook)
        (progn
          (remove-hook 'before-save-hook #'whitespace-cleanup)
          (message ">>> boem: Whitespace cleanup on save turned OFF"))
      (add-hook 'before-save-hook #'whitespace-cleanup)
      (message ">>> boem: Whitespace cleanup on save turned ON")))
  (global-set-key (kbd "C-c t w") #'boem/toggle-whitespace-cleanup-on-save))

;; =========================

(use-package man
  :ensure nil
  :commands (man)
  :config
  ;; does not obey `display-buffer-alist'
  (setq Man-notify-method 'pushy))

;; =========================

(use-package minibuffer
  :ensure nil
  :custom
  (completion-auto-help t)
  (completion-auto-select t)
  ;; Emacs-31
  (completion-eager-update t)
  ;; Emacs-31 (if not using icomplete, t is way cooler)
  (completion-eager-display t)
  ;; Emacs-31
  (minibuffer-visible-completions 'up-down)
  (completion-ignore-case t)
  (completion-show-help nil)
  (completion-styles '(partial-completion flex initials))
  (completion-category-overrides '((eglot-capf (styles flex-noinsert))))
  (completions-format 'one-column)
  (completions-max-height 10)
  (completions-sort 'historical)
  (enable-recursive-minibuffers t)
  (read-buffer-completion-ignore-case t)
  (read-file-name-completion-ignore-case t)
  :config
  ;; Emacs-31
  (when (>= emacs-major-version 31)
    (keymap-set minibuffer-visible-completions-up-down-map "C-n"
                #'minibuffer-next-completion)
    (keymap-set minibuffer-visible-completions-up-down-map "C-p"
                #'minibuffer-previous-completion))

  (defun boem/flex-noinsert-try-completion (string table pred point)
    "Flex `try-completion' that never auto-extends the input on TAB.

  The stock `flex' completion style does two jobs: it filters
  candidates by fuzzy (subsequence) match, and its `try-completion'
  merges the surviving candidates, inserting their common expansion
  into the buffer.  With `tab-always-indent' set to `complete' that
  merge means TAB silently types a candidate (often a far, wrong one)
  *before* the *Completions* list is shown.  Eglot's own
  `eglot--dumb-flex' avoids the merge but gives no relevance sorting.

  This wrapper keeps flex's filtering and scoring (so prefix matches
  sort first, fuzzy ones last) but suppresses the merge:

    - no candidates           -> nil   (no match)
    - exactly one candidate   -> complete it fully (TAB still finishes
                                 a unique completion)
    - two or more candidates  -> return STRING unchanged, so TAB only
                                 pops the *Completions* list and lets
                                 you pick, inserting nothing.

  Registered as the `flex-noinsert' style and used for Eglot's
  `eglot-capf' category via `completion-category-overrides'.  See
  `completion-flex-all-completions' and
  `completion--flex-adjust-metadata' for the filtering/sorting it
  piggybacks on.

  STRING, TABLE, PRED and POINT are the usual `try-completion' args."
    (let ((all (completion-flex-all-completions string table pred point)))
      (cond
       ((null all) nil)
       ((= (safe-length all) 1)
        (let ((sole (car all)))
          (if (string= sole string) t (cons sole (length sole)))))
       (t (cons string point)))))

  ;; Register the `flex-noinsert' style: same filtering/sorting as
  ;; `flex', but `boem/flex-noinsert-try-completion' as its try function.
  (add-to-list 'completion-styles-alist
               '(flex-noinsert
                 boem/flex-noinsert-try-completion
                 completion-flex-all-completions
                 "Flex matching that never extends input on TAB."))
  ;; Reuse flex's metadata tweak so *Completions* sorts by flex score.
  (put 'flex-noinsert 'completion--adjust-metadata
       'completion--flex-adjust-metadata)

  ;; Keep the cursor out of the read-only portions of theminibuffer
  (setq minibuffer-prompt-properties
        '(read-only t intangible t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Keep minibuffer lines unwrapped, long lines like on M-S-y will be truncated
  (add-hook 'minibuffer-setup-hook
            (lambda () (setq truncate-lines t)))

  (minibuffer-depth-indicate-mode 1)
  (minibuffer-electric-default-mode 1))

;; =========================

(use-package newsticker
  :ensure nil
  :defer t
  :custom
  ;; Only fetches when first opening (avoids unwanted fetching/ui
  ;; locking while doing other things later)
  (newsticker-retrieval-interval 0)
  (newsticker-treeview-treewindow-width 40)
  (newsticker-dir (boem/data-path 'newsticker-dir))
  (newsticker-retrieval-method (if (executable-find "wget") 'extern 'intern))
  (newsticker-wget-arguments
   '("--quiet"
     "--no-hsts"
     "--output-document=-"
     "--append-output=/dev/null")))

;; =========================

;;; │ ELECTRIC-PAIR
(use-package electric-pair
  :ensure nil
  :defer
  :hook (after-init-hook . electric-pair-mode))

;; =========================

(use-package paren
  :ensure nil
  :hook (after-init-hook . show-paren-mode)
  :custom
  (show-paren-delay 0)
  (show-paren-style 'mixed)
  ;; show matches within window splits
  (show-paren-context-when-offscreen t))

;; =========================

(use-package proced
  :ensure nil
  :defer t
  :custom
  (proced-enable-color-flag t)
  (proced-tree-flag t)
  (proced-auto-update-flag 'visible)
  (proced-auto-update-interval 1)
  (proced-descend t)
  (proced-format 'medium) ;; can be changed interactively with `F'
  (proced-filter 'user)   ;; can be changed interactively with `f'
  :config
  ;; FIXME: Remove this once https://debbugs.gnu.org/cgi/bugreport.cgi?bug=80898 lands
  (when (eq system-type 'darwin)
    (defvar boem--proced-ps-cache (make-hash-table))
    (defvar boem--proced-ps-timer nil)

    (defun boem/proced-ps-do-refresh ()
      (make-process
       :name "proced-ps-refresh"
       :buffer (generate-new-buffer " *proced-ps-temp*")
       :command '("env" "LC_ALL=C" "ps" "-axo" "pid=,%cpu=,%mem=")
       :noquery t
       :sentinel
       (lambda (proc _event)
         (when (eq (process-status proc) 'exit)
           (let ((new-cache (make-hash-table)))
             (with-current-buffer (process-buffer proc)
               (goto-char (point-min))
               (while (not (eobp))
                 (when (looking-at
                        (rx
                         (* blank)
                         (group (+ digit))
                         (+ blank)
                         (group (+ (any digit ?.)))
                         (+ blank)
                         (group (+ (any digit ?.)))))
                   (puthash (string-to-number (match-string 1))
                            (cons (string-to-number (match-string 2))
                                  (string-to-number (match-string 3)))
                            new-cache))
                 (forward-line 1)))
             (kill-buffer (process-buffer proc))
             (setq boem--proced-ps-cache new-cache))))))

    (defun boem/proced-pcpu (pid)
      (car (gethash pid boem--proced-ps-cache)))
    (defun boem/proced-pmem (pid)
      (cdr (gethash pid boem--proced-ps-cache)))

    (add-hook 'proced-mode-hook
              (lambda ()
                (unless (file-remote-p default-directory)
                  (setq boem--proced-ps-timer
                        (run-with-timer 0 2 #'boem/proced-ps-do-refresh)))))
    (add-hook 'kill-buffer-hook
              (lambda ()
                (when (and (derived-mode-p 'proced-mode)
                           (timerp boem--proced-ps-timer))
                  (cancel-timer boem--proced-ps-timer)
                  (setq boem--proced-ps-timer nil))))

    (setq proced-custom-attributes
          (list (lambda (attrs)
                  (unless (file-remote-p default-directory)
                    (when-let* ((pid (cdr (assq 'pid attrs)))
                                (v (boem/proced-pcpu pid)))
                      (cons 'pcpu v))))
                (lambda (attrs)
                  (unless (file-remote-p default-directory)
                    (when-let* ((pid (cdr (assq 'pid attrs)))
                                (v (boem/proced-pmem pid)))
                      (cons 'pmem v))))))))

;; =========================

(use-package org
  :ensure nil
  :defer t
  :mode (("\\.org\\'" . org-mode)
         ("\\.org_archive\\'" . org-mode))
  :config
  (setopt org-export-backends '(ascii html icalendar latex odt md))
  (setq
   ;; I have basic org directory
   org-directory boem-user-org-directory

   ;; LaTeX related
   org-format-latex-options (plist-put org-format-latex-options :scale 1.7)
   org-preview-latex-default-process 'dvisvgm

   ;; Start collapsed for speed
   org-startup-folded t

   ;; Look and feel
   org-src-fontify-natively t

   org-refile-targets '((org-agenda-files :level . 1))

   ;; Edit settings
   org-auto-align-tags nil
   org-tags-column 0
   org-fold-catch-invisible-edits 'show-and-error
   org-special-ctrl-a/e t
   org-insert-heading-respect-content t

   ;; Org styling, hide markup etc.
   org-hide-emphasis-markers t
   org-pretty-entities t
   ;; We want the above but no _ subscripts ^ superscripts
   org-use-sub-superscripts nil

   ;; Org habit
   org-habit-graph-column 90

   ;; Org crypt
   org-tags-exclude-from-inheritance (quote ("Encrypt"))
   org-crypt-tag-matcher "Encrypt"
   ;; GPG key to use for encryption
   ;; Either the Key ID or set to nil to use symmetric encryption.
   org-crypt-key nil

   ;; Agenda related
   org-agenda-files (list boem-user-org-directory)
   org-agenda-include-diary nil
   org-agenda-time-leading-zero t
   org-agenda-tags-column 0
   org-agenda-block-separator ?─
   org-agenda-time-grid
   '((daily today require-timed)
     (0700 0900 1100 1300 1500 1700 1900 2100)
     " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
   org-agenda-current-time-string
   "◀── now ─────────────────────────────────────────────────"
   org-agenda-prefix-format '((agenda . " %i %-20:c%?-12t% s") (todo . " %i %-20:c")
                              (tags . " %i %-20:c") (search . " %i %-20:c"))
   org-agenda-custom-commands
   '(
     ("A" "Daily agenda and top priority tasks"
      ((tags-todo "*"
                  ((org-agenda-skip-function
                    `(org-agenda-skip-entry-if
                      'notregexp
                      ,(format
                        "\\[#%s\\]"
                        (char-to-string
                         org-priority-highest))))
                   (org-agenda-block-separator nil)
                   (org-agenda-overriding-header "Важни задаци")))
       (agenda ""
               ((org-agenda-time-grid nil)
                (org-agenda-start-on-weekday nil)
                (org-agenda-span 1)
                (org-agenda-show-all-dates nil)
                (org-scheduled-past-days 365)
                (org-scheduled-delay-days 1)
                (org-agenda-block-separator nil)
                (org-agenda-entry-types '(:scheduled))
                (org-agenda-skip-function
                 '(org-agenda-skip-entry-if
                   'todo 'done))
                (org-agenda-day-face-function
                 (lambda (date)
                   'org-agenda-date))
                (org-agenda-format-date "")
                (org-agenda-overriding-header "\nПрошли неурађени задаци")))
       (agenda ""
               ((org-agenda-span 1)
                (org-deadline-warning-days 0)
                (org-agenda-block-separator nil)
                (org-scheduled-past-days 0)
                (org-agenda-day-face-function
                 (lambda (date)
                   'org-agenda-date))
                (org-agenda-overriding-header "\nДанашњи распоред")))
       (agenda ""
               ((org-agenda-start-on-weekday nil)
                (org-agenda-start-day "+1d")
                (org-agenda-span 5)
                (org-deadline-warning-days 0)
                (org-agenda-block-separator nil)
                (org-agenda-skip-function
                 '(org-agenda-skip-entry-if
                   'todo 'done))
                (org-agenda-overriding-header "\nНаредних пет дана")))
       (agenda ""
               ((org-agenda-time-grid nil)
                (org-agenda-start-on-weekday nil)
                (org-agenda-start-day "+4d")
                (org-agenda-span 14)
                (org-agenda-show-all-dates nil)
                (org-deadline-warning-days 0)
                (org-agenda-block-separator nil)
                (org-agenda-entry-types '(:deadline))
                (org-agenda-skip-function
                 '(org-agenda-skip-entry-if
                   'todo 'done))
                (org-agenda-overriding-header "\nПредстојећи рокови (+14д)"))))))

   ;; Org capture
   org-capture-templates
   `(("t" "Todo" entry
      (file+headline ,(expand-file-name "todos.org" boem-user-org-directory) "Tasks")
      "* TODO %?\n  %i\n  %a")
     ("j" "Journal" entry
      (file+olp+datetree ,(expand-file-name "journal.org" boem-user-org-directory))
      "* %?\Zapisano  %U\n  %i\n  %a")
     ("l" "Link" plain
      (file+headline ,(expand-file-name "za-citanje.org" boem-user-org-directory)
       "Nepročitani tabovi") "  - %c %U"))
   )

  ;; Ellipsis styling
  (setq org-ellipsis " ▼ ")
  (set-face-attribute 'org-ellipsis nil :inherit 'default :box nil)

  ;; Org crypt
  (org-crypt-use-before-save-magic)

  ;; Keywords
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
     ("REPORT" :foreground "yellow" :weight bold)))

  (setq
   ;; Anytime a task is marked done the line states `CLOSED: [timestamp]
   org-log-done 'time
   org-global-properties '(("Effort_ALL". "0 0:30 1:00 2:00 3:00 4:00 6:00 8:00"))
   org-columns-default-format "%50ITEM(Task) %6Effort{:} %10CLOCKSUM %SCHEDULED"
   org-tag-alist boem-org-tags
   org-clock-sound t
   org-enforce-todo-dependencies t
   org-enforce-todo-checkbox-dependencies t
   org-archive-location (concat
                         (expand-file-name "archive.org" boem-user-org-directory)
                         "::* From %s")
   org-imenu-depth 3)

  ;; Load babel only when org loads
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
     (graphql . t)))
  (setq org-confirm-babel-evaluate nil))

;; =========================

(use-package speedbar
  :ensure nil
  :bind
  (("M-I" . (lambda () ;; Toggles / focuses speedbar on side window
              (interactive)
              (speedbar-window)       ;; EMACS-31
              (let ((win (get-buffer-window speedbar-buffer)))
                (when win
                  (select-window win))))))
  :custom
  ;; Emacs-31
  (speedbar-window-default-width 25)
  ;; Emacs-31
  (speedbar-window-max-width 25)
  (speedbar-show-unknown-files t)
  (speedbar-directory-unshown-regexp "^$")
  (speedbar-indentation-width 2)
  (speedbar-use-images t)
  (speedbar-update-flag nil)
  :config
  (setq speedbar-expand-image-button-alist
        '(("<+>" . ezimage-directory) ;; previously ezimage-directory-plus
          ("<->" . ezimage-directory-minus)
          ("< >" . ezimage-directory)
          ("[+]" . ezimage-page-plus)
          ("[-]" . ezimage-page-minus)
          ("[?]" . ezimage-page)
          ("[ ]" . ezimage-page)
          ("{+}" . ezimage-directory-plus) ;; previously ezimage-box-plus
          ("{-}" . ezimage-directory-minus) ;; previously ezimage-box-minus
          ("<M>" . ezimage-mail)
          ("<d>" . ezimage-document-tag)
          ("<i>" . ezimage-info-tag)
          (" =>" . ezimage-tag)
          (" +>" . ezimage-tag-gt)
          (" ->" . ezimage-tag-v)
          (">"   . ezimage-tag)
          ("@"   . ezimage-tag-type)
          ("  @" . ezimage-tag-type)
          ("*"   . ezimage-checkout)
          ("#"   . ezimage-object)
          ("!"   . ezimage-object-out-of-date)
          ("//"  . ezimage-label)
          ("%"   . ezimage-lock))))

;; =========================

(use-package time
  :ensure nil
  ;; If we'd like to see it on the mode-line
  ;; :hook (after-init-hook . display-time-mode)
  :custom
  (world-clock-time-format "%A %d %B %T %Z")
  (display-time-day-and-date t)
  (display-time-default-load-average nil)
  (display-time-mail-string "")
  (zoneinfo-style-world-list
   '(("Europe/Belgrade" "Belgrade")
     ("America/New_York" "New York")
     ("UTC" "UTC")
     ("Europe/Paris" "Paris")
     ("Europe/Moscow" "Moscow"))))

;; =========================

(use-package uniquify
  :ensure nil
  :config
  (setq uniquify-buffer-name-style 'forward
        uniquify-separator " • "
        uniquify-min-dir-content 3
        uniquify-strip-common-suffix t
        uniquify-ignore-buffers-re "^\\*"))

;; =========================

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

;; =========================

(use-package which-key
  :defer t
  :ensure nil
  :hook
  (after-init-hook . which-key-mode)
  :config
  (setq which-key-separator " "
        which-key-prefix-prefix "… "
        which-key-max-display-columns 3
        which-key-idle-delay 0.5
        which-key-idle-secondary-delay 0.25
        which-key-add-column-padding 1
        which-key-max-description-length 40)

  ;; Inspired by: https://gist.github.com/mmarshall540/a12f95ab25b1941244c759b1da24296d
  ;;
  ;; By default, Which-key doesn't give much help for prefix-keys. It
  ;; either shows the generic description, "+prefix", or the name of a
  ;; prefix-command, which usually isn't as descriptive as we'd like.
  ;;
  ;; Here are some descriptions for the default bindings in `global-map'
  ;; and `org-mode-map'.
  (which-key-add-key-based-replacements
    "<f1> 4" "help-other-win"
    "<f1>" "help"
    "<f2>" "2column"
    "C-c" "mode-and-user"
    "C-c !" "flymake"
    "C-c g" "git-gutter"
    "C-h 4" "help-other-win"
    "C-h" "help"
    "C-x 4" "other-window"
    "C-x 5" "other-frame"
    "C-x 6" "2-column"
    "C-x 8" "insert-special"
    "C-x 8 ^" "superscript (⁰, ¹, ², …)"
    "C-x 8 _" "subscript (₀, ₁, ₂, …)"
    "C-x 8 a" "arrows & æ (←, →, ↔, æ)"
    "C-x 8 e" "emojis (🫎, 🇧🇷, 🇮🇹, …)"
    "C-x 8 *" "common symbols ( , ¡, €, …)"
    "C-x 8 =" "macron (Ā, Ē, Ḡ, …)"
    "C-x 8 N" "macron (№)"
    "C-x 8 O" "macron (œ)"
    "C-x 8 ~" "tilde (~, ã, …)"
    "C-x 8 /" "stroke (÷, ≠, ø, …)"
    "C-x 8 ." "dot (·, ż)"
    "C-x 8 ," "cedilla (¸, ç, ą, …)"
    "C-x 8 '" "acute (á, é, í, …)"
    "C-x 8 `" "grave (à, è, ì, …)"
    "C-x 8 \"" "quotation/dieresis (\", ë, ß, …)"
    "C-x 8 1" "†, 1/…"
    "C-x 8 2" "‡"
    "C-x 8 3" "3/…"
    "C-x C-k C-q" "kmacro-counters"
    "C-x C-k C-r a" "kmacro-add"
    "C-x C-k C-r" "kmacro-register"
    "C-x C-k" "keyboard-macros"
    "C-x RET" "encoding/input"
    "C-x a i" "abbrevs-inverse-add"
    "C-x a" "abbrevs"
    "C-x n" "narrowing"
    "C-x p" "projects"
    "C-x r" "reg/rect/bkmks"
    "C-x t ^" "tab-bar-detach"
    "C-x t" "tab-bar"
    "C-x v M" "vc-mergebase"
    "C-x v b" "vc-branch"
    "C-x v" "version-control"
    "C-x w ^" "window-detach"
    "C-x w" "window-extras"
    "C-x x" "buffer-extras"
    "C-x" "extra-commands"
    "M-g" "goto-map"
    "M-s h" "search-highlight"
    "M-s" "search-map")

  ;; Upon loading, the built-in `page-ext' package turns "C-x C-p" into
  ;; a prefix-key. If you know of other built-in packages that have
  ;; this behavior, please let me know, so I can add them.
  (with-eval-after-load 'page-ext
    (which-key-add-key-based-replacements
      "C-x C-p" "page-extras"))

  ;; Org-mode provides some additional prefix-keys in `org-mode-map'.
  (with-eval-after-load 'org
    (which-key-add-keymap-based-replacements org-mode-map
      "C-c \"" "org-plot"
      "C-c C-v" "org-babel"
      "C-c C-x" "org-extra-commands")))

;; =========================

(use-package webjump
  :defer t
  :ensure nil
  :bind ("M-o /" . boem/webjump-eww)
  :custom
  (webjump-sites
   '(("DuckDuckGo"     . [simple-query "https://www.duckduckgo.com" "https://www.duckduckgo.com/?q=" ""])
     ("DuckDuckGoNoAI" . [simple-query "https://noai.duckduckgo.com" "https://noai.duckduckgo.com/?q=" ""])
     ("DuckDuckAI"     . [simple-query "https://duck.ai" "https://duck.ai/?q=" ""])
     ("DuckDuckGoImg"  . [simple-query "https://www.duckduckgo.com" "https://www.duckduckgo.com/?iar=images&q=" ""])
     ("Google"         . [simple-query "https://www.google.com" "https://www.google.com/search?q=" ""])
     ("Qwant"          . [simple-query "https://www.qwant.com" "https://www.qwant.com/?t=web&q=" ""])
     ("YouTube"        . [simple-query "https://www.youtube.com/feed/subscriptions" "https://www.youtube.com/results?search_query=" ""])
     ("Claude"         . [simple-query "https://claude.ai/new" "https://claude.ai/new?q=" ""])
     ("ChatGPT"        . [simple-query "https://chatgpt.com" "https://chatgpt.com/?q=" ""])))
  :config
  (defun boem/webjump-eww (&optional arg)
    "Run `webjump' optionally forcing the internal browser (EWW)."
    (interactive "P")
    (require 'eww)
    (let ((webjump-use-internal-browser arg))
      (call-interactively #'webjump))))

;; =========================

;;; ├──────────────────── NON TREESITTER AREA
;;; │ SASS-MODE
(use-package scss-mode
  :mode "\\.sass\\'"
  :hook
  ((scss-mode-hook . (lambda ()
                       (setq indent-tabs-mode nil))))
  :defer t)

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

;;; ├──────────────────── TREESITTER AREA
(use-package ruby-ts-mode
  :ensure nil
  :mode "\\.rb\\'"
  :mode "Rakefile\\'"
  :mode "Gemfile\\'"
  :custom
  (add-to-list
   'treesit-language-source-alist
   '(ruby "https://github.com/tree-sitter/tree-sitter-ruby" "master" "src"))
  (ruby-indent-level 2)
  (ruby-deep-indent-paren nil)
  (ruby-indent-tabs-mode nil)
  :init
  (eval-after-load "hideshow"
    '(add-to-list
      'hs-special-modes-alist
      `(ruby-ts-mode
        ;; Block start
        ,(rx (or "def" "class" "module" "do" "{" "[" "if" "else" "unless"))
        ;; Block end
        ,(rx (or "}" "]" "end"))
        ; Comment start
        ,(rx (or "#" "=begin"))
        forward-sexp nil))))

(use-package elixir-ts-mode
  :ensure nil
  :mode (("\\.ex\\'" . elixir-ts-mode)
         ("\\.exs\\'" . elixir-ts-mode))
  :defer t
  :config
  (add-to-list
   'treesit-language-source-alist
   '(elixir "https://github.com/elixir-lang/tree-sitter-elixir" "main" "src"))
  (eval-after-load "hideshow"
    '(add-to-list
      'hs-special-modes-alist
      `(elixir-ts-mode
        ;; Block start
        ,(rx
          (or
           "def" "defp" "defmodule" "do" "{" "[" "if" "else" "unless" "describe" "setup" "test"
           ))
        ;; Block end
        ,(rx (or "}" "]" "end"))
        ;; Comment start
        ,(rx (or "#"))
        )))
  (add-hook 'elixir-ts-mode-hook
            (lambda ()
              (add-hook 'before-save-hook #'eglot-format nil t))))

(use-package heex-ts-mode
  :ensure nil
  :mode "\\.heex\\'"
  :defer t
  :config
  (add-to-list
   'treesit-language-source-alist
   '(heex "https://github.com/phoenixframework/tree-sitter-heex" "main" "src"))
  (add-hook 'elixir-ts-mode-hook
            (lambda ()
              (add-hook 'before-save-hook #'eglot-format-buffer nil t))))

(use-package css-ts-mode
  :ensure nil
  :mode ("\\.css\\'" . css-mode)
  :init
  (progn
    (setq css-indent-offset 2)))

(use-package js-ts-mode
  ;; js-ts-mode is autoloaded; js.el (and its js-base-mode parent)
  ;; loads lazily on first .js/.jsx file
  :ensure nil
  :mode "\\.jsx?\\'"
  :defer t
  :hook
  ((js-ts-mode-hook . (lambda ()
                        (setq indent-tabs-mode nil))))
  :custom
  (js-indent-level 2)
  :config
  (add-to-list
   'treesit-language-source-alist
   '(javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src"))
  (add-to-list
   'treesit-language-source-alist
   '(jsdoc "https://github.com/tree-sitter/tree-sitter-jsdoc" "master" "src")))

(use-package json-ts-mode
  :mode "\\.json\\'"
  :defer t
  :hook
  ((json-ts-mode-hook . (lambda ()
                          (setq indent-tabs-mode nil)))))

(defun boem/add-jsdoc-in-typescript-ts-mode ()
  "Add jsdoc treesitter rules to typescript as a host language.
As seen on:
https://www.reddit.com/r/emacs/comments/1kfblch/need_help_with_adding_jsdoc_highlighting_to"
  ;; This code was copied from emacs-solo which copied it from js.el
  ;; (js-ts-mode), with minimal modifications.
  (when (treesit-ready-p 'typescript)
    (when (treesit-ready-p 'jsdoc t)
      (setq-local treesit-range-settings
                  (treesit-range-rules
                   :embed 'jsdoc
                   :host 'typescript
                   :local t
                   `(((comment) @capture (:match ,(rx bos "/**") @capture)))))
      (setq c-ts-common--comment-regexp
            (rx (or "comment" "line_comment" "block_comment" "description")))

      (defvar my/treesit-font-lock-settings-jsdoc
        (treesit-font-lock-rules
         :language 'jsdoc
         :override t
         :feature 'document
         '((document) @font-lock-doc-face)

         :language 'jsdoc
         :override t
         :feature 'keyword
         '((tag_name) @font-lock-constant-face)

         :language 'jsdoc
         :override t
         :feature 'bracket
         '((["{" "}"]) @font-lock-bracket-face)

         :language 'jsdoc
         :override t
         :feature 'property
         '((type) @font-lock-type-face)

         :language 'jsdoc
         :override t
         :feature 'definition
         '((identifier) @font-lock-variable-face)))
      (setq-local treesit-font-lock-settings
                  (append treesit-font-lock-settings my/treesit-font-lock-settings-jsdoc)))))

(use-package typescript-ts-mode
  :mode "\\.ts\\'"
  :defer t
  :hook
  ((typescript-ts-mode-hook .
                            (lambda ()
                              (setq indent-tabs-mode nil)
                              (boem/add-jsdoc-in-typescript-ts-mode))))
  :custom
  (typescript-indent-level 2)
  :config
  (add-to-list
   'treesit-language-source-alist
   ;; Emacs-31 this is now defined on mode code
   '(typescript
     "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src"))
  (unbind-key "M-." typescript-ts-base-mode-map))

(use-package tsx-ts-mode
  :mode "\\.tsx\\'"
  :defer t
  :hook
  ((tsx-ts-mode-hook .
                     (lambda ()
                       (setq indent-tabs-mode nil)
                       (boem/add-jsdoc-in-typescript-ts-mode))))
  :custom
  (typescript-indent-level 2)
  :config
  (add-to-list
   'treesit-language-source-alist
   ;; Emacs-31 this is now defined on mode code
   '(tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src"))
  (unbind-key "M-." typescript-ts-base-mode-map))

(use-package bash-ts-mode
  :ensure nil
  :mode "\\.\\(sh\\|bash\\)\\'"
  :defer t
  :config
  (add-to-list
   'treesit-language-source-alist
   '(bash "https://github.com/tree-sitter/tree-sitter-bash" "master" "src")))

(use-package toml-ts-mode
  :ensure toml-ts-mode
  :mode "\\.toml\\'"
  :defer t
  :config
  (add-to-list
   'treesit-language-source-alist
   ;; Emacs-31 this is now defined on mode code
   '(toml "https://github.com/ikatyang/tree-sitter-toml" "master" "src")))

;;  As first proposed here:
;;  https://lists.gnu.org/archive/html/emacs-devel/2025-02/msg00810.html
(use-package markdown-ts-mode
  :ensure nil
  :mode ("\\.md\\'" "\\.mdx\\'" "\\.markdown\\'")
  :init (load-library "markdown-ts-mode"))

(use-package yaml-ts-mode
  :ensure yaml-ts-mode
  :mode "\\.ya?ml\\'"
  :defer t
  :config
  (add-to-list
   'treesit-language-source-alist
   ;; Emacs-31 this is now defined on mode code
   '(yaml "https://github.com/tree-sitter-grammars/tree-sitter-yaml" "master" "src")))

(use-package dockerfile-ts-mode
  :ensure dockerfile-ts-mode
  :mode "\\Dockerfile.*\\'"
  :defer t
  :config
  (add-to-list
   'treesit-language-source-alist
   ;; Emacs-31 this is now defined on mode code
   '(dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile" "main" "src")))

(provide 'internal-packages)
