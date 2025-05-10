;;; init.el --- Bosko's Emacs initialization file

;;; Commentary:
;;

;; Make startup faster by reducing the frequency of garbage
;; collection.  The default is 800 kilobytes. Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

(defvar boem-init-root
  (expand-file-name (file-name-directory load-file-name)))

(add-to-list 'load-path (expand-file-name "lisp" boem-init-root))
(add-to-list 'load-path (expand-file-name "experiments" boem-init-root))

(load "init-basic")

(message "%s, starting up Emacs" boem-current-user)

(add-to-list 'treesit-extra-load-path (expand-file-name "tree-sitter" boem-user-data-directory))

(setq-default ;; xdisp.c
 cursor-type 'box
 tab-width 2
 indent-tabs-mode nil
 frame-title-format "emacs - %b"
 scroll-step 1
 scroll-margin 0
 scroll-conservatively 10000
 scroll-up-aggressively 0.01
 scroll-down-aggressively 0.01
 show-trailing-whitespace t
 auto-window-vscroll nil
 ;; scroll-preserve-screen-position t
 scroll-preserve-screen-position 1
 delete-by-moving-to-trash t
 eshell-prompt-regexp "^> "
 completion-ignored-extensions
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
   ".hi" ".pyx" ".map")
 mode-line-format '("%e"
                    mode-line-front-space
                    mode-line-mule-info
                    mode-line-client
                    mode-line-modified
                    mode-line-remote
                    mode-line-frame-identification
                    mode-line-buffer-identification
                    "  "
                    mode-line-position
                    mode-line-modes
                    "  "
                    (vc-mode vc-mode)
                    "  "
                    mode-line-misc-info
                    mode-line-end-spaces))

(setq
 package-user-dir boem-user-package-directory
 custom-file (expand-file-name "custom.el" boem-user-data-directory)
 ;; Use data directory for storing some packages data
 transient-history-file (expand-file-name "transient/history.el" boem-user-data-directory)
 org-persist-directory (expand-file-name "org-persist" boem-user-data-directory)
 project-list-file (expand-file-name "projects" boem-user-data-directory)
 bookmark-default-file (expand-file-name "bookmarks" boem-user-data-directory)
 newsticker-dir (expand-file-name "newsticker" boem-user-data-directory)

 package-enable-at-startup nil
 inhibit-splash-screen t
 inhibit-startup-message t
 backup-directory-alist `((".*" . ,temporary-file-directory))
 auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
 tags-revert-without-query t
 ;; Set standard indent to 2 rather then 4
 standard-indent 2
 ;; Prevent Emacs from extending file when
 ;; pressing down arrow at end of buffer.
 next-line-add-newlines nil
 grep-command "grep -n -r --exclude=\\*{.git,TAGS,sqlite3,log,tmp/\\*,vendor/bundle/\\*,.bundle/\\*} -e "
 use-package-idle-interval 1.5
 fill-column 80
 echo-keystrokes 0.1
 calendar-week-start-day 1
 eshell-hist-ignoredups t
 eshell-destroy-buffer-when-process-dies t
 epa-armor t
 dired-listing-switches "-alh"
 dired-dwim-target t
 use-package-compute-statistics t
 scroll-preserve-screen-position t
 calendar-latitude 44.787197
 calendar-longitude 20.457273
 calendar-location-name "Београд, Србија"
 calendar-day-name-array ["недеља" "понедељак" "уторак" "среда" "четвртак" "петак" "субота"]
 calendar-day-abbrev-array ["не" "по" "ут" "ср" "че" "пе" "су"]
 calendar-day-header-array ["не" "по" "ут" "ср" "че" "пе" "су"]
 calendar-month-name-array ["Јануар" "Фебруар" "Март" "Април" "Мај" "Јун" "Јул"
                            "Август" "Септембар" "Октобар" "Новембар" "Децембар"]
 view-read-only t
 isearch-allow-scroll t
 isearch-lazy-count t
 lazy-count-prefix-format nil
 lazy-count-suffix-format "   (%s/%s)"
 ediff-keep-variants nil
 ediff-split-window-function #'split-window-horizontally
 ediff-window-setup-function #'ediff-setup-windows-plain
 load-prefer-newer t
 ;; Improve lsp-mode performances
 read-process-output-max (* 1024 1024)
 ;; Add following two lines in ~/.gnupg/gpg-agent.conf
 ;; allow-emacs-pinentry
 ;; allow-loopback-pinentry
 ;; and run:
 ;; gpgconf --reload gpg-agent
 epa-pinentry-mode 'loopback
 ;; modus-themes configuration
 modus-themes-slanted-constructs t
 modus-themes-hl-line '(accented intense)
 modus-themes-mode-line '(3d accented)
 modus-themes-prompts '(background intense bold)
 modus-themes-paren-match '(intense bold)
 modus-themes-bold-constructs nil
 ansi-color-for-comint-mode t
 locale-coding-system 'utf-8
 default-file-name-coding-system 'utf-8
 ;; Treat clipboard input as UTF-8 string first; compound text next, etc.
 x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

(setq ring-bell-function
      (lambda ()
        (let ((orig-bg (face-background 'mode-line)))
          (set-face-background 'mode-line "#F2804F")
          (run-with-idle-timer 0.1 nil
                               (lambda (fg) (set-face-background 'mode-line fg))
                               orig-bg))))

(setq eshell-prompt-function
      (lambda nil
        (let ((path (abbreviate-file-name (eshell/pwd))))
          (concat
           (format
            (propertize "(%s@%s)[%s]\n>" 'face '(:weight bold))
            (propertize (user-login-name) 'face '(:foreground "cyan"))
            (propertize (system-name) 'face '(:foreground "cyan"))
            (propertize path 'face
                        `(:foreground ,(if (= (user-uid) 0) "red" "green") :weight bold)))
           " "))))

(put 'dired-find-alternate-file 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(dolist (hook '(eshell-mode-hook
                term-mode-hook
                eww-mode-hook
                erc-mode-hook
                shell-mode-hook
                magit-diff-mode-hook
                ibuffer-mode-hook
                dired-mode-hook
                occur-mode-hook
                docker-cli-mode-hook
                help-mode-hook))
  (add-hook hook '(lambda() (setq show-trailing-whitespace nil))))

(add-hook 'sql-interactive-mode-hook
          '(lambda()
             (boem-set-proper-sql-prompt-regex)
             (setq-local show-trailing-whitespace nil)
             (toggle-truncate-lines)))

(add-hook 'dired-mode-hook #'dired-hide-details-mode)

(add-hook 'js-ts-mode-hook
          '(lambda() (setq-local js-indent-level 2)))

(add-hook 'json-ts-mode-hook
          '(lambda()
             (setq-local js-indent-level 2)
             (hs-minor-mode)))

;; "Kill up to, but not including ARGth occurrence of CHAR. (fn arg char)"
(autoload 'zap-up-to-char "misc" 'interactive)

(autoload 'inf-ruby-minor-mode "inf-ruby" "Run an inferior Ruby process" t)
(add-hook 'ruby-mode-hook 'inf-ruby-minor-mode)

(global-set-key (kbd "M-o d") 'duplicate-dwim)
(global-set-key (kbd "M-o b") 'boem-switch-to-previous-buffer)
(global-set-key (kbd "M-o e") 'boem-pop-eshell-bottom)
(global-set-key (kbd "M-o t") 'eat)
(global-set-key (kbd "M-o r") 'boem-restclient)
(global-set-key (kbd "M-o k a") 'boem-kill-user-buffers)
(global-set-key (kbd "C-x C-m") 'execute-extended-command)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-S-<return>") 'boem-insert-line-above)
(global-set-key (kbd "S-<return>") 'boem-insert-line)
(global-set-key (kbd "M-Z") 'zap-up-to-char)
(global-set-key (kbd "C-x /") 'boem-comment-uncomment)
(global-set-key (kbd "C-c <left>") 'windmove-left)
(global-set-key (kbd "C-c <up>") 'windmove-up)
(global-set-key (kbd "C-c <right>") 'windmove-right)
(global-set-key (kbd "C-c <down>") 'windmove-down)
(global-set-key (kbd "M-l") 'scroll-down-line)
(global-set-key (kbd "M-k") 'scroll-up-line)
(global-set-key (kbd "C-c r a") 'inf-ruby-console-auto)

;; Keep syntax highlighting in current line.
(set-face-foreground 'highlight nil)

(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

(setq ansi-color-for-comint-mode t)

;;;; mule / conding.c
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(set-language-environment 'utf-8)
(prefer-coding-system 'utf-8-unix)

(setq locale-coding-system 'utf-8)
(setq default-file-name-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
;; backwards compatibility as default-buffer-file-coding-system
;; is deprecated in 23.2.
(if (boundp buffer-file-coding-system)
    (setq buffer-file-coding-system 'utf-8)
  (setq default-buffer-file-coding-system 'utf-8))

;; Treat clipboard input as UTF-8 string first; compound text next, etc.
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

(column-number-mode t)
(global-hl-line-mode 1)
(global-so-long-mode 1)
(global-completion-preview-mode)
(delete-selection-mode)

(defalias 'yes-or-no-p 'y-or-n-p)

(package-initialize)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; Do not let warning and compile error buffers to pop-up
(add-to-list 'display-buffer-alist
             '("\\`\\*\\(Warnings\\|Compile-Log\\)\\*\\'"
               (display-buffer-no-window)
               (allow-no-window . t)))

(defun prot/keyboard-quit-dwim ()
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

(define-key global-map (kbd "C-g") #'prot/keyboard-quit-dwim)

;;; Theme
(if (eq nil (display-graphic-p))
    (load-theme 'modus-operandi t)
  (progn
    (load-theme 'modus-vivendi t)
    (set-face-attribute 'modus-themes-completion-selected nil :background "gray34")))

(define-key global-map (kbd "<f5>") #'modus-themes-toggle)

(require 'use-package)

(if (find-font (font-spec :name "DejaVuSansMono Nerd Font Mono"))
    (set-frame-font "DejaVuSansMono Nerd Font Mono 12" t t))

(load "init-packages")

(if (string-equal system-type "darwin")
    (pinentry-start))

(load custom-file 'no-error)

(if (fboundp 'fringe-mode)
    (fringe-mode 9))

(message "%s, Emacs started in %s with %d garbage collections."
         boem-current-user
         (format "%.2f seconds"
                 (float-time
                  (time-subtract (current-time) before-init-time)))
         gcs-done)

(put 'narrow-to-region 'disabled nil)

;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1000 1000))

(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))

(set-input-method 'cyrillic-serbian)

(provide 'init)

;;; init.el ends here
