;;; init.el --- Bosko's Emacs initialization file -*- lexical-binding: t; -*-

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

(defcustom boem-gui-theme 'modus-vivendi
  "Emacs theme to load when GUI is started"
  :type 'symbol
  :group 'boem)

(defcustom boem-tui-theme nil
  "Emacs theme to load when started in terminal"
  :type 'symbol
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

;; This enables, otherwise disabled commands so Emacs does not ask the
;; question:
;; "Do you want to use this comand anyway?"
(put 'dired-find-alternate-file 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

(add-hook 'js-ts-mode-hook
          '(lambda() (setq-local js-indent-level 2)))

(add-hook 'json-ts-mode-hook
          '(lambda()
             (setq-local js-indent-level 2)))

(package-initialize)
(add-to-list 'package-archives '("melpa" . "https://releases.melpa.org/packages/") t)

;; (load "init-packages")

(load custom-file 'no-error 'nomessage)

(message ">>> %s, Emacs started in %s with %d garbage collections."
         boem-current-user
         (format "%.2f seconds"
                 (float-time
                  (time-subtract (current-time) before-init-time)))
         gcs-done)

(set-input-method 'cyrillic-serbian)

(provide 'init)

;;; init.el ends here
