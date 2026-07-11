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
(use-package nerd-icons
  :ensure t)

(use-package nerd-icons-ibuffer
  :ensure t
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

(use-package nerd-icons-dired
  :ensure t
  :hook
  (dired-mode . nerd-icons-dired-mode))

(use-package nerd-icons-completion
  :ensure t
  :after marginalia
  :config
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

(use-package ob-restclient
  :ensure t
  :after (ob))

(use-package ob-graphql
  :ensure t
  :after (ob))

(use-package org-superstar
  :after (org)
  :ensure t
  :init
  (add-hook 'org-mode-hook (lambda() (org-superstar-mode)))
  :config
  (setq org-superstar-special-todo-items t))

(use-package inf-ruby
  :ensure t
  :commands ruby-ts-mode
  :config
  (progn
    (inf-ruby-minor-mode +1))
  :init
  (progn
    (add-hook 'inf-ruby-mode-hook (lambda() (setq show-trailing-whitespace nil)))))

(use-package exunit
  :ensure t
  :config
  (add-hook 'elixir-mode-hook 'exunit-mode)
  (setq transient-default-level 5))

(provide 'external-packages)
