;;; early-init.el --- Boem Configuration --- Early Init  -*- lexical-binding: t; -*-
;;
;; Author: Boško Ivanišević
;; URL: https://github.com/bosko/boem
;; Package-Requires: ((emacs "30.1"))
;; Keywords: config
;; SPDX-License-Identifier: GPL-3.0-or-later
;;

;;; Commentary:
;;  Early init configuration for Emacs Boem
;;

;;; Code:
(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'noerror 'nomessage)

(setq package-enable-at-startup nil)

;; Set eln-cache dir
(when (boundp 'native-comp-eln-load-path)
  (startup-redirect-eln-cache
   (expand-file-name
    "eln-cache" (expand-file-name "data" user-emacs-directory))))

;; Delay garbage collection while Emacs is booting
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; Schedule garbage collection sensible defaults for after booting
(add-hook 'after-init-hook
          (lambda ()
            (setq gc-cons-threshold (* 100 1024 1024)
                  gc-cons-percentage 0.1)))

;; Single VC backend inscreases booting speed
(setq vc-handled-backends '(Git))

;; Always start Emacs and new frames maximized
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(if (eq system-type 'darwin)
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark)))

;; Better Window Management handling
(setq frame-resize-pixelwise t
      frame-inhibit-implied-resize t)

;; Disables unused UI Elements
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'tooltip-mode) (tooltip-mode -1))

;; Avoid raising the *Messages* buffer if anything is still without
;; lexical bindings
(setq warning-minimum-level :error)
(setq warning-suppress-types '((lexical-binding)))
