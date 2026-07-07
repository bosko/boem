;;; init.el --- Bosko's Emacs initialization file

;;; Commentary:
;;

(defcustom boem-preferred-font-name "DejaVuSansMono Nerd Font Mono"
  "The name of the font that will be used.
Examples: `JetBrainsMono Nert Font' or FiraCode Nerd Font Mono"
  :type 'string
  :group 'boem
  )

(defcustom boem-preferred-font-sizes '(130 105)
  "List of default font sizes (first for macOS, second for GNU/Linux)."
  :type '(repeat integer)
  :group 'boem)

(defcustom boem-data-directory
  (expand-file-name "data/" user-emacs-directory)
  "Base directory for boem data files.
All entries in `boem-data-paths' are resolved relative to this
directory."
  :type 'string
  :group 'boem)

(defvar boem-init-root
  (expand-file-name (file-name-directory load-file-name)))

(defvar boem-current-user
  (getenv (if (equal system-type 'windows-nt) "USERNAME" "USER")))

(defvar boem-data-paths
  '(;; Files:
    (bookmark-file               . "bookmarks")
    (ielm-history-file-name      . "ielm-history.eld")
    (project-list-file           . "projects")
    (recentf-save-file           . "recentf")
    (savehist-file               . "history")
    (save-place-file             . "saveplace")
    (transient-history-file      . "transient/history.el")
    (transient-levels-file       . "transient/levels.el")
    (transient-values-file       . "transient/values.el")
    (tramp-persistency-file-name . "tramp")
    (nsm-settings-file           . "network-security.data")
    ;; Directories:
    (auto-saves                  . "auto-saves/")
    (auto-saves-sessions         . "auto-saves/sessions/")
    (shared-game-score-directory . "games/")
    (multisession-directory      . "multisession/")
    (url-configuration-directory . "url/")
    (rcirc-log-directory         . "rcirc/logs/")
    (erc-log-channels-directory  . "erc/logs/")
    (erc-image-cache-directory   . "erc/images/")
    (image-dired-dir             . "image-dired/")
    (newsticker-dir              . "newsticker/")
    (org-persist-dir             . "org-persists/")
    (yt-subs                     . "yt-subs")
    (tree-sitter-dir             . "tree-sitter/"))
  "Alist of (KEY . RELATIVE-PATH) for Emacs Solo cache locations.
RELATIVE-PATH is resolved against `boem-cache-directory'.
A trailing slash on RELATIVE-PATH marks the entry as a directory.")

(defconst boem-version-string
  (mapconcat 'identity
             (mapcar
              #'(lambda(x) (number-to-string x))
              (version-to-list emacs-version))
             ".")
  "Emacs version as string.")

(defconst boem-user-package-directory
  (expand-file-name
   (format "packages/%s" boem-version-string) user-emacs-directory))
(defconst boem-user-data-directory (expand-file-name "data" user-emacs-directory))
(defconst boem-user-themes-directory (expand-file-name "themes" user-emacs-directory))
(defconst boem-user-org-directory (expand-file-name "~/org-files"))

(add-to-list 'custom-theme-load-path boem-user-themes-directory)
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "experiments" user-emacs-directory))

;;; Load function definitions
(require 'init-basic)

;;; Ensure all directories are present
(boem/ensure-data-dirs)
(make-directory boem-user-package-directory t)
(make-directory boem-user-data-directory t)
(make-directory boem-user-themes-directory t)
(make-directory boem-user-org-directory t)

(add-to-list 'treesit-extra-load-path (boem/data-path 'tree-sitter-dir))

(require 'internal-packages)
(require 'external-packages)

(message "%s, starting up Emacs" boem-current-user)

;; (setq-default ;; xdisp.c
;;  cursor-type 'box
;;  frame-title-format "emacs - %b"
;;  auto-window-vscroll nil
;;  eshell-prompt-regexp "^> "
;;  always using left-to-right languages
;;  mode-line-format '("%e"
;;                     mode-line-front-space
;;                     mode-line-mule-info
;;                     mode-line-client
;;                     mode-line-modified
;;                     mode-line-remote
;;                     mode-line-frame-identification
;;                     mode-line-buffer-identification
;;                     "  "
;;                     mode-line-position
;;                     mode-line-modes
;;                     "  "
;;                     (vc-mode vc-mode)
;;                     "  "
;;                     mode-line-misc-info
;;                     mode-line-end-spaces))

(setq
 ;; auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
 ;; tags-revert-without-query t
 ;; eshell-hist-ignoredups t
 ;; eshell-destroy-buffer-when-process-dies t
 ;; dired-listing-switches "-alh"
 ;; dired-dwim-target t
 ;; isearch-allow-scroll t
 ;; isearch-lazy-count t
 ;; lazy-count-prefix-format nil
 ;; lazy-count-suffix-format "   (%s/%s)"
 ;; ediff-keep-variants nil
 ;; ediff-split-window-function #'split-window-horizontally
 ;; ediff-window-setup-function #'ediff-setup-windows-plain
 ;; modus-themes configuration
 ;; modus-themes-bold-constructs t
 ;; modus-themes-prompts '(extrabold italic)
 ;; locale-coding-system 'utf-8
)

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
                ghostel-mode-hook
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

(add-hook 'dired-mode-hook #'dired-hide-details-mode)

(add-hook 'js-ts-mode-hook
          '(lambda() (setq-local js-indent-level 2)))

(add-hook 'json-ts-mode-hook
          '(lambda()
             (setq-local js-indent-level 2)))

(add-hook 'prog-mode-hook #'hs-minor-mode)

;; "Kill up to, but not including ARGth occurrence of CHAR. (fn arg char)"
(autoload 'zap-up-to-char "misc" 'interactive)

(autoload 'inf-ruby-minor-mode "inf-ruby" "Run an inferior Ruby process" t)
(add-hook 'ruby-mode-hook 'inf-ruby-minor-mode)

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
(setq buffer-file-coding-system 'utf-8)

;; Treat clipboard input as UTF-8 string first; compound text next, etc.
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

(global-hl-line-mode 1)
(global-so-long-mode 1)
(global-completion-preview-mode 1)
(delete-selection-mode 1)

(defalias 'yes-or-no-p 'y-or-n-p)

(package-initialize)
(add-to-list 'package-archives '("melpa" . "https://releases.melpa.org/packages/") t)

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
(if (display-graphic-p)
    (load-theme 'modus-vivendi t))

(define-key global-map (kbd "<f5>") #'modus-themes-toggle)

;; (require 'use-package)

;; (load "init-packages")

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
