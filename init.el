;;; init.el --- Bosko's Emacs initialization file

;;; Commentary:
;;

;;; Code:

(defvar boem-current-user
  (getenv
   (if (equal system-type 'windows-nt) "USERNAME" "USER")))

(defvar boem-init-root (expand-file-name
                   (file-name-directory load-file-name)))
(add-to-list 'load-path (expand-file-name "lisp" boem-init-root))

(message "%s, starting up Emacs" boem-current-user)

(load "init-basic")

(setq
 package-enable-at-startup nil
 package-user-dir boem-user-package-directory
 inhibit-splash-screen t
 inhibit-startup-message t
 backup-directory-alist `((".*" . ,temporary-file-directory))
 auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
 custom-file (expand-file-name "custom.el" boem-user-data-directory)
 tags-revert-without-query t
 ;; Set standard indent to 2 rather then 4
 standard-indent 2
 ;; Prevent Emacs from extending file when
 ;; pressing down arrow at end of buffer.
 next-line-add-newlines nil
 grep-command "grep -n -r --exclude=\\*{.git,TAGS,sqlite3,log,tmp/\\*,vendor/bundle/\\*,.bundle/\\*} -e "
 gc-cons-threshold 20000000
 use-package-idle-interval 1.5
 fill-column 80
 echo-keystrokes 0.1
 bookmark-default-file (expand-file-name "bookmarks" boem-user-data-directory)
 calendar-week-start-day 1
 eshell-hist-ignoredups t
 eshell-destroy-buffer-when-process-dies t
 dired-dwim-target t
 epa-pinentry-mode 'loopback
 epa-armor t
 dired-listing-switches "-alh"
 dired-dwim-target t)

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
            (propertize path 'face `(:foreground ,(if (= (user-uid) 0) "red" "green") :weight bold)))
           " "))))

(put 'dired-find-alternate-file 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(setq-default ;; xdisp.c
 cursor-type 'bar
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
   ".hi" ".pyx" ".map"))

(add-hook 'eshell-mode-hook
          '(lambda () (setq show-trailing-whitespace nil)))
(add-hook 'sql-interactive-mode-hook
          '(lambda ()
             (boem-set-proper-sql-prompt-regex)
             (setq show-trailing-whitespace nil)
             (toggle-truncate-lines)))
(add-hook 'term-mode-hook
          '(lambda () (setq show-trailing-whitespace nil)))
(add-hook 'eww-mode-hook
          '(lambda () (setq show-trailing-whitespace nil)))

"Kill up to, but not including ARGth occurrence of CHAR. (fn arg char)"
(autoload 'zap-up-to-char "misc" 'interactive)

(autoload 'inf-ruby-minor-mode "inf-ruby" "Run an inferior Ruby process" t)
(add-hook 'ruby-mode-hook 'inf-ruby-minor-mode)

(global-set-key (kbd "C-x /") 'comment-or-uncomment-region)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-S-<return>") 'boem-insert-line-above)
(global-set-key (kbd "S-<return>") 'boem-insert-line)
(global-set-key (kbd "C-c d") 'boem-duplicate-line-or-region)
(global-set-key (kbd "M-Z") 'zap-up-to-char)
(global-set-key (kbd "C-c k a") 'boem-kill-user-buffers)
(global-set-key (kbd "C-x /") 'boem-comment-uncomment)
(global-set-key (kbd "C-c b") 'boem-switch-to-previous-buffer)
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
(delete-selection-mode)

(defalias 'yes-or-no-p 'y-or-n-p)

(eval-and-compile
  (require 'package)
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
  (package-initialize t))

(boem-install-package-if-needed 'use-package)
(boem-install-package-if-needed 'railscasts-theme)

(eval-when-compile (require 'use-package))

(load "init-packages")

(with-eval-after-load 'em-term
  (nconc eshell-visual-commands
         '("htop" "pinentry-curses" "watch"))
  (setq eshell-visual-subcommands
        '(("git" "log" "l" "lol" "diff" "d" "dc" "show")
          ("rails" "c")
          ("sudo" "vi"))))

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; Make multi-cursor cursors number more visible
;; in the mode line
(set-face-foreground 'font-lock-warning-face "red")
(set-face-attribute 'mode-line nil :box '(:line-width 2 :color "#009bff"))

(load custom-file 'no-error)

(if (display-graphic-p)
    (progn
      (load-theme 'railscasts t)
      (custom-theme-set-faces
       'railscasts
       '(hl-line ((t (:foreground nil :background "#505050"))))
       '(region ((t (:background "#708238"))))))
  (load-theme 'manoj-dark t))

(if (fboundp 'fringe-mode)
    (fringe-mode 9))

;; For displaying emojis on OS X.
;; Font Symbola must be downloaded from
;; http://xahlee.info/comp/unicode_font_download.html
;; and installed for this to work
(when (member "Symbola" (font-family-list))
  (set-fontset-font t 'unicode "Symbola" nil 'prepend))

(message "%s, Emacs started in %s" boem-current-user (format "%.1f seconds"
                                                        (float-time
                                                         (time-subtract (current-time) before-init-time))))

(provide 'init)

;;; init.el ends here
(put 'narrow-to-region 'disabled nil)
