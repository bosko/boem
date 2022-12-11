;; Set eln-cache dir
(when (boundp 'native-comp-eln-load-path)
  (startup-redirect-eln-cache
   (expand-file-name
    "eln-cache" (expand-file-name "data" user-emacs-directory))))
