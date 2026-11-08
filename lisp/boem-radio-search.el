;;; boem-radio-search.el --- Search radio stations via radio-browser.info -*- lexical-binding: t; -*-
;;
;; Author: Boško Ivanišević

;;; Commentary:
;;
;; Search radio stations by name using the radio-browser.info API
;; (https://de1.api.radio-browser.info/) with consult.
;;
;; The search starts once at least `boem-radio-search-min-input'
;; characters are typed and is sent after a pause in typing of
;; `boem-radio-search-delay' seconds.  Requests are done
;; asynchronously with the built-in `url' library only.
;;
;; Usage: M-x boem/radio-search
;;   RET      - play the selected station with mpv
;;   C-u M-x boem/radio-search, RET - copy the station URL instead
;;   Embark actions on a candidate (category `boem-radio-station'):
;;     p - play
;;     w - copy URL
;;
;; Playback reuses the mpv process of `emacs-solo-m3u', so starting a
;; station here stops a station started from the M3U list and vice
;; versa.  Stop playback with M-x boem/radio-stop.

;;; Code:

(require 'url)
(require 'url-util)
(require 'json)
(require 'subr-x)

(defvar url-http-response-status)
(defvar url-http-end-of-headers)

(declare-function consult--read "consult")
(declare-function consult--async-pipeline "consult")
(declare-function consult--async-min-input "consult")
(declare-function consult--async-throttle "consult")
(declare-function m3u-visualizer--kill-mpv "emacs-solo-m3u")
(defvar m3u-visualizer--mpv-process)
(defvar embark-keymap-alist)
(defvar embark-general-map)

(defgroup boem-radio-search nil
  "Search radio stations using radio-browser.info."
  :group 'multimedia)

(defcustom boem-radio-search-api-url "https://de1.api.radio-browser.info"
  "Base URL of the radio-browser.info API server."
  :type 'string
  :group 'boem-radio-search)

(defcustom boem-radio-search-delay 1.0
  "Seconds of typing inactivity after which the search is sent."
  :type 'number
  :group 'boem-radio-search)

(defcustom boem-radio-search-min-input 3
  "Minimum number of characters before the search starts."
  :type 'natnum
  :group 'boem-radio-search)

(defcustom boem-radio-search-limit 100
  "Maximum number of stations returned by a search."
  :type 'natnum
  :group 'boem-radio-search)

(defcustom boem-radio-search-name-width 40
  "Width of the station name column; longer names are truncated."
  :type 'natnum
  :group 'boem-radio-search)

(defcustom boem-radio-search-player-args '("--no-terminal")
  "Extra arguments passed to mpv when playing a station."
  :type '(repeat string)
  :group 'boem-radio-search)

(defvar boem-radio-search--history nil
  "Minibuffer history for `boem/radio-search'.")

;;;; HTTP request

(defun boem/radio-search--request-url (name)
  "Return the API search URL for stations matching NAME."
  (concat (string-trim-right boem-radio-search-api-url "/")
          "/json/stations/search?"
          (url-build-query-string
           `((name ,name)
             (hidebroken "true")
             (order "clickcount")
             (reverse "true")
             (limit ,boem-radio-search-limit)))))

(defun boem/radio-search--parse-response ()
  "Parse the HTTP response in the current buffer.
Return a list of (NAME . URL) pairs or signal an error."
  (unless (and (integerp url-http-response-status)
               (<= 200 url-http-response-status 299))
    (error "HTTP status %S" url-http-response-status))
  (let* ((body (decode-coding-string
                (buffer-substring-no-properties
                 url-http-end-of-headers (point-max))
                'utf-8))
         (stations (json-parse-string body
                                      :object-type 'alist
                                      :array-type 'list
                                      :null-object nil
                                      :false-object nil)))
    (delq nil
          (mapcar (lambda (station)
                    (let ((name (string-trim (or (alist-get 'name station) "")))
                          (url (or (alist-get 'url_resolved station)
                                   (alist-get 'url station))))
                      (when (and url (not (string-empty-p url)))
                        (cons (if (string-empty-p name) url name) url))))
                  stations))))

(defun boem/radio-search--fetch (name callback)
  "Search stations matching NAME asynchronously.
CALLBACK is called with (STATIONS ERROR) where STATIONS is a list of
\(NAME . URL) pairs.  Return the request buffer."
  (url-retrieve
   (boem/radio-search--request-url name)
   (lambda (status)
     (let ((buffer (current-buffer)))
       (unwind-protect
           (if-let* ((err (plist-get status :error)))
               (funcall callback nil err)
             (condition-case err
                 (funcall callback (boem/radio-search--parse-response) nil)
               (error (funcall callback nil err))))
         (when (buffer-live-p buffer)
           (let ((kill-buffer-query-functions nil))
             (kill-buffer buffer))))))
   nil t t))

(defun boem/radio-search--cancel-request (buffer)
  "Abort the request associated with BUFFER."
  (when (buffer-live-p buffer)
    (when-let* ((proc (get-buffer-process buffer)))
      (delete-process proc))
    (let ((kill-buffer-query-functions nil))
      (kill-buffer buffer))))

;;;; Candidates

(defun boem/radio-search--safe-fetch (sink name callback)
  "Call `boem/radio-search--fetch' with NAME and CALLBACK.
Errors signaled while starting the request are reported through SINK."
  (condition-case err
      (boem/radio-search--fetch name callback)
    (error
     (funcall sink [indicator failed])
     (message "Radio search failed: %s" (error-message-string err))
     nil)))

(defun boem/radio-search--format (station)
  "Return a completion candidate for STATION, a (NAME . URL) pair."
  (let* ((name (car station))
         (url (cdr station))
         (cand (concat (truncate-string-to-width
                        name boem-radio-search-name-width 0 ?\s "…")
                       "  "
                       (propertize url 'face 'font-lock-comment-face))))
    (add-text-properties 0 (length cand)
                         `(boem-radio-name ,name boem-radio-url ,url)
                         cand)
    cand))

(defun boem/radio-search--candidate-url (cand)
  "Return the stream URL of candidate CAND."
  (or (get-text-property 0 'boem-radio-url cand)
      (car (last (split-string cand "[ \t]+" t)))))

(defun boem/radio-search--candidate-name (cand)
  "Return the full station name of candidate CAND."
  (or (get-text-property 0 'boem-radio-name cand)
      (string-trim (string-remove-suffix
                    (boem/radio-search--candidate-url cand) cand))))

(defun boem/radio-search--async-fetch ()
  "Consult async function sending the input to the radio-browser API."
  (lambda (sink)
    (let (request last-input)
      (lambda (action)
        (pcase action
          ((pred stringp)
           (funcall sink action)
           (unless (equal action last-input)
             (boem/radio-search--cancel-request request)
             (setq last-input action)
             (funcall sink [indicator running])
             (let (this)
               (setq this
                     (boem/radio-search--safe-fetch
                      sink
                      action
                      (lambda (stations err)
                        ;; Ignore responses of superseded requests.
                        (when (eq this request)
                          (setq request nil)
                          (funcall sink 'flush)
                          (if err
                              (progn
                                (funcall sink [indicator failed])
                                (message "Radio search failed: %s"
                                         (error-message-string
                                          (if (consp err) err (list 'error err)))))
                            (when stations
                              (funcall sink (mapcar #'boem/radio-search--format
                                                    stations)))
                            (funcall sink [indicator finished]))
                          (funcall sink 'refresh)))))
               (setq request this)))
           nil)
          ((or 'cancel 'destroy)
           (boem/radio-search--cancel-request request)
           (setq request nil last-input nil)
           (funcall sink action))
          (_ (funcall sink action)))))))

;;;; Actions

(defun boem/radio-search--mpv-sentinel (proc _event)
  "Forget PROC when the mpv process ends."
  (unless (process-live-p proc)
    (when (and (boundp 'm3u-visualizer--mpv-process)
               (eq m3u-visualizer--mpv-process proc))
      (setq m3u-visualizer--mpv-process nil))))

(defun boem/radio-search--play-url (url &optional name)
  "Play stream URL with mpv.  NAME is used in the message.
Reuses the mpv process slot of `emacs-solo-m3u' so only one station
plays at a time."
  (unless (executable-find "mpv")
    (user-error "The mpv executable is not found"))
  (boem/radio-stop t)
  (let ((proc (apply #'start-process "mpv-stream" "*mpv*" "mpv"
                     (append boem-radio-search-player-args (list url)))))
    (set-process-query-on-exit-flag proc nil)
    (set-process-sentinel proc #'boem/radio-search--mpv-sentinel)
    (setq m3u-visualizer--mpv-process proc)
    (message "Playing %s" (or name url))))

(defun boem/radio-stop (&optional quiet)
  "Stop the currently playing radio station.
When QUIET is non-nil, do not report anything."
  (interactive)
  (let ((proc (and (boundp 'm3u-visualizer--mpv-process)
                   m3u-visualizer--mpv-process)))
    (if (not (process-live-p proc))
        (unless quiet (message "No radio station is playing"))
      (if (fboundp 'm3u-visualizer--kill-mpv)
          (m3u-visualizer--kill-mpv)
        (delete-process proc)
        (setq m3u-visualizer--mpv-process nil))
      (unless quiet (message "Radio stopped")))))

(defun boem-radio-search-play (cand)
  "Play the station of candidate CAND."
  (interactive "sStation: ")
  (boem/radio-search--play-url (boem/radio-search--candidate-url cand)
                       (boem/radio-search--candidate-name cand)))

(defun boem-radio-search-copy-url (cand)
  "Copy the stream URL of candidate CAND to the kill ring."
  (interactive "sStation: ")
  (let ((url (boem/radio-search--candidate-url cand)))
    (kill-new url)
    (message "Copied %s" url)))

(with-eval-after-load 'embark
  (defvar-keymap boem-radio-search-embark-map
    :doc "Embark actions for radio stations."
    :parent embark-general-map
    "p" #'boem-radio-search-play
    "w" #'boem-radio-search-copy-url)
  (add-to-list 'embark-keymap-alist
               '(boem-radio-station . boem-radio-search-embark-map)))

;;;; Command

;;;###autoload
(defun boem/radio-search (&optional copy)
  "Search radio stations by name and play the selected one.
With prefix argument COPY, copy the station URL instead of playing it.
More actions are available through `embark-act'."
  (interactive "P")
  (require 'consult)
  (let ((cand (consult--read
               (consult--async-pipeline
                (consult--async-min-input boem-radio-search-min-input)
                (consult--async-throttle 0 boem-radio-search-delay)
                (boem/radio-search--async-fetch))
               :prompt "Radio station: "
               :category 'boem-radio-station
               :require-match t
               :sort nil
               :history '(:input boem-radio-search--history))))
    (when (and cand (not (string-empty-p cand)))
      (if copy
          (boem-radio-search-copy-url cand)
        (boem-radio-search-play cand)))))

(provide 'boem-radio-search)
;;; boem-radio-search.el ends here
