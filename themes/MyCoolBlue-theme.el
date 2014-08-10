;;; CoolBlue-theme.el --- Custom face theme for Emacs

;; Copyright (C) 2010 Steve S.

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(deftheme MyCoolBlue
  "")

(let ((class '((class color) (min-colors 89)))) 
  (custom-theme-set-faces
   'MyCoolBlue
   `(default ((,class (:background "#14212e" :foreground "#d4d4d4"))))
   `(cursor ((,class (:background "#ffff00" :foreground "#000000"))))
   `(mode-line ((,class (:background "#000000" :foreground "#0498da"))))
   `(mode-line-inactive ((,class (:background "#000000" :foreground "#0498da"))))
   `(fringe ((,class (:background "#000000")))) 
   `(highlight ((,class (:background "#333333" :foreground "#ffffff"))))
   `(minibuffer-prompt ((,class (:foreground "#ffcc33"))))
   `(font-lock-builtin-face ((,class (:foreground "#ffff00"))))
   `(font-lock-comment-face ((,class (:foreground "#7eb0c9"))))
   `(font-lock-constant-face ((,class (:foreground "#009bff"))))
   `(font-lock-function-name-face ((,class (:foreground "#e55a04"))))
   `(font-lock-keyword-face ((,class (:bold t :foreground "#009bff"))))
   `(font-lock-string-face ((,class (:foreground "#c2b56e"))))
   `(font-lock-type-face ((,class (:foreground "#ffcc33"))))
   `(font-lock-variable-name-face ((,class (:foreground "#d1682c"))))
   `(font-lock-warning-face ((,class (:foreground "#ff0000" :weight bold))))
   `(isearch ((,class (:background "#ffcc33" :foreground "#000000"))))
   `(lazy-highlight ((,class (:background "#000000"))))
   `(link ((,class (:foreground "#009bff" :underline t))))
   `(link-visited ((,class (:foreground "#bd00bd" :underline t))))
   `(button ((,class (:underline t))))
   `(header-line ((,class (:background "#000000" :foreground "#0498da"))))))

(provide-theme 'MyCoolBlue)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; CoolBlue-theme.el  ends here
