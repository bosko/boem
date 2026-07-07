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
   ("M-s g" . grep)
   ("C-x C-m" . execute-extended-command)
   ("C-x C-b" . ibuffer)
   ("C-S-<return>" . boem-insert-line-above)
   ("S-<return>" . boem-insert-line)
   ("M-Z" . zap-up-to-char)
   ("C-x /" . boem-comment-uncomment)
   ("M-l" . scroll-down-line)
   ("M-k" . scroll-up-line)
   ("C-c r a" . inf-ruby-console-auto))
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
  (org-persist-directory (boem/data-path 'org-persists-dir))
  (package-user-dir boem-user-package-directory)
  (pixel-scroll-precision-mode t)
  (pixel-scroll-precision-use-momentum nil)
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
  ; So vertical splits are preferred
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
  (use-package-hook-name-suffix nil)
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
  ; used by M-x grep
  (grep-command "rg -nS --no-heading ")
  ; used if M-x rgrep uses find (default in grep-find-template)
  (grep-find-ignored-directories
   '("SCCS" "RCS" "CVS" "MCVS" ".src" ".svn" ".jj" ".git" ".hg" ".bzr" "_MTN" "_darcs" "{arch}" "node_modules" "build" "dist"))
  ; used by M-x rgrep (dropping find when using rg)
  (grep-find-template "rg <C> --null -nH -e <R> <D>")
  :config
  ;; Make C-x 5 o repeatable
  (defvar-keymap frame-repeat-map
    :repeat t
    "o" #'other-frame
    "n" #'make-frame
    "d" #'delete-frame)
  (put 'other-frame 'repeat-map 'frame-repeat-map)

  ;; Makes everything accept utf-8 as default, so buffers with tsx and so
  ;; won't ask for encoding (because undecided-unix) every single keystroke
  (modify-coding-system-alist 'file "" 'utf-8)

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
                    (number-to-string (length package-activated-list)))))

  (message ">>> boem: init time %s" (emacs-init-time)))

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

(provide 'internal-packages)
