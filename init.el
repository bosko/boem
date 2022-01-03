;;; init.el --- Bosko's Emacs initialization file

;;; Commentary:
;;

;; Make startup faster by reducing the frequency of garbage
;; collection.  The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

;;; Code:
;;; Temporary problem with 26.2 version on OS x
(if (equal emacs-major-version 26)
    (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

(defvar boem-current-user
  (getenv
   (if (equal system-type 'windows-nt) "USERNAME" "USER")))

(defvar boem-init-root (expand-file-name
                   (file-name-directory load-file-name)))
(add-to-list 'load-path (expand-file-name "lisp" boem-init-root))

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
                (org-agenda-format-date "%A %-e %B %Y")
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
 use-package-idle-interval 1.5
 fill-column 80
 echo-keystrokes 0.1
 bookmark-default-file (expand-file-name "bookmarks" boem-user-data-directory)
 calendar-week-start-day 1
 eshell-hist-ignoredups t
 eshell-destroy-buffer-when-process-dies t
 epa-armor t
 dired-listing-switches "-alh"
 dired-dwim-target t
 use-package-compute-statistics t
 scroll-preserve-screen-position t
 calendar-latitude 45.267136
 calendar-longitude 19.833549
 calendar-location-name "Нови Сад, Србија"
 calendar-day-name-array ["недеља" "понедељак" "уторак" "среда" "четвртак" "петак" "субота"]
 calendar-day-abbrev-array ["не" "по" "ут" "ср" "че" "пе" "су"]
 calendar-day-header-array ["не" "по" "ут" "ср" "че" "пе" "су"]
 calendar-month-name-array ["Јануар" "Фебруар" "Март" "Април" "Мај" "Јун" "Јул"
                            "Август" "Септембар" "Октобар" "Новембар" "Децембар"])

;; Improve lsp-mode performances on version 27
(if (equal emacs-major-version 27)
    (setq read-process-output-max (* 1024 1024)))

;; Add following two lines in ~/.gnupg/gpg-agent.conf
;; allow-emacs-pinentry
;; allow-loopback-pinentry
;; and run:
;; gpgconf --reload gpg-agent
(if (equal emacs-major-version 27)
    (setq epg-pinentry-mode 'loopback)
  (setq epa-pinentry-mode 'loopback))

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

(add-hook 'eshell-mode-hook
          '(lambda () (setq show-trailing-whitespace nil)))
(add-hook 'term-mode-hook
          '(lambda () (setq show-trailing-whitespace nil)))
(add-hook 'eww-mode-hook
          '(lambda () (setq show-trailing-whitespace nil)))
(add-hook 'erc-mode-hook
          '(lambda () (setq show-trailing-whitespace nil)))

(add-hook 'sql-interactive-mode-hook
          '(lambda ()
             (boem-set-proper-sql-prompt-regex)
             (setq show-trailing-whitespace nil)
             (toggle-truncate-lines)))

(add-hook 'dired-mode-hook #'dired-hide-details-mode)

;; "Kill up to, but not including ARGth occurrence of CHAR. (fn arg char)"
(autoload 'zap-up-to-char "misc" 'interactive)

(autoload 'inf-ruby-minor-mode "inf-ruby" "Run an inferior Ruby process" t)
(add-hook 'ruby-mode-hook 'inf-ruby-minor-mode)

(global-set-key (kbd "M-o d") 'boem-duplicate-line-or-region)
(global-set-key (kbd "M-o b") 'boem-switch-to-previous-buffer)
(global-set-key (kbd "M-o e") 'boem-pop-eshell-bottom)
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
(global-set-key (kbd "C-c p f") 'project-find-file)

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

(package-initialize)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(boem-install-package-if-needed 'use-package)
(boem-install-package-if-needed 'railscasts-theme)

(require 'use-package)

(load "init-packages")

(if (string-equal system-type "darwin")
    (pinentry-start))

(with-eval-after-load 'em-term
  (nconc eshell-visual-commands
         '("htop" "pinentry-curses" "watch"))
  (setq eshell-visual-subcommands
        '(("git" "log" "l" "lol" "diff" "d" "dc" "show")
          ("rails" "c")
          ("sudo" "vi"))))

(load-theme 'modus-vivendi t)

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

(if (find-font (font-spec :name "Ubuntu Mono"))
    (set-frame-font "Ubuntu Mono-14" t t))

;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1000 1000))

(provide 'init)

;;; init.el ends here
