;;; init.el --- Bosko's Emacs initialization file -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:
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

(defcustom boem-gui-theme "modus-vivendi"
  "Emacs theme to load when GUI is started."
  :type 'string
  :group 'boem)

(defcustom boem-tui-theme nil
  "Emacs theme to load when started in terminal."
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

;; In "emacs" package we set package-user-dir and after that we have
;; to initialize package so Emacs properly adds all subdirs to the
;; load path.
(package-initialize)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(require 'external-packages)

;; (setq-default ;; xdisp.c
;;  cursor-type 'box
;;  auto-window-vscroll nil
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

;; This enables, otherwise disabled commands so Emacs does not ask the
;; question: "Do you want to use this comand anyway?"
(put 'dired-find-alternate-file 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

(load custom-file 'no-error 'nomessage)

(message ">>> %s, Emacs started in %s with %d garbage collections."
         boem-current-user
         (format "%.2f seconds"
                 (float-time
                  (time-subtract (current-time) before-init-time)))
         gcs-done)

(set-input-method 'cyrillic-serbian)

(require 'boem-weather)
(require 'ibuffer-vc)
(require 'emacs-solo-ace-window)
(require 'emacs-solo-container)
(require 'emacs-solo-dired-mpv)
(require 'emacs-solo-exec-path-from-shell)
(require 'emacs-solo-highlight-keywords)
(require 'emacs-solo-m3u)
(require 'emacs-solo-mode-line)
(require 'emacs-solo-project-select)
(require 'emacs-solo-rainbow-delimiters)
(require 'emacs-solo-weather)

(provide 'init)

;;; init.el ends here
