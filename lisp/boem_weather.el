;;; boem_weather.el
;; Author: Bosko Ivanisevic
;; Version: 0.1

(require 'json)
(require 'url)
(require 'cl-lib)
(require 'subr-x)
(require 'nerd-icons)
(defgroup boem-weather nil
  "Weather display using open-meteo.com."
  :group 'applications)

(defcustom boem-weather-latitude 44.804
  "Latitude used for the forecast."
  :type 'number
  :group 'boem-weather)

(defcustom boem-weather-longitude 20.4651
  "Longitude used for the forecast."
  :type 'number
  :group 'boem-weather)

(defcustom boem-weather-time-zone "Europe/Belgrade"
  "Time zone used for forecats."
  :type 'string
  :group 'boem-weather)

(defcustom boem-weather-keybinding (kbd "C-c w")
  "Global key binding for `boem-weather'."
  :type 'key-sequence
  :group 'boem-weather)

(defcustom boem-weather-daily-model "best_match"
  "Daily weather forecast model"
  :type 'string
  :group 'boem-weather)

;;; ----------------------------------------------------------------
;;; Internal state
;;; ----------------------------------------------------------------
(defconst boem-weather--buffer-name "*Boem-Weather*")

(defconst boem-weather--current-param
  '(
    ("current" "weather_code,temperature_2m,apparent_temperature,wind_speed_10m,wind_direction_10m,precipitation_probability,precipitation,pressure_msl,relative_humidity_2m")
    ("forecast_days" 1)
    ))

(defconst boem-weather--hourly-param
  '(
    ("hourly" "temperature_2m,apparent_temperature,precipitation_probability,precipitation,rain,weather_code,wind_speed_10m,wind_direction_10m")
    ("forecast_days" 1)
    ))

(defconst boem-weather--daily-param
  '(
    ("daily" "weather_code,temperature_2m_max,temperature_2m_min,wind_speed_10m_max,wind_direction_10m_dominant,sunrise,sunset,precipitation_probability_max")
    ("forecatst_days" 7)
    ))

(defvar boem-weather--data nil)
(defvar boem-weather--display-interval 'current)
(defvar boem-weather--frame nil)

;;; ----------------------------------------------------------------
;;; Icons and descriptions
;;; ----------------------------------------------------------------
(defun boem-weather--wi (name &optional face) (nerd-icons-wicon name :face face))
(defun boem-weather--oct(name &optional face) (nerd-icons-octicon name :face face))

(defconst boem-weather--code->icon
  '((0  . "nf-weather-day_sunny") (1  . "nf-weather-day_sunny_overcast")
    (2 . "nf-weather-day_cloudy") (3 . "nf-weather-cloudy")
    (45 . "nf-weather-fog") (48 . "nf-weather-fog")
    (51 . "nf-weather-sprinkle") (53 . "nf-weather-sprinkle")
    (55 . "nf-weather-sprinkle") (56 . "nf-weather-rain_mix")
    (57 . "nf-weather-rain_mix") (61 . "nf-weather-showers")
    (63 . "nf-weather-rain") (65 . "nf-weather-rain")
    (66 . "nf-weather-rain_mix") (67 . "nf-weather-rain_mix")
    (71 . "nf-weather-snow") (73 . "nf-weather-snow")
    (75 . "nf-weather-snow") (77 . "nf-weather-snow")
    (80 . "nf-weather-showers") (81 . "nf-weather-showers")
    (82 . "nf-weather-showers") (85 . "nf-weather-snow")
    (86 . "nf-weather-snow") (95 . "nf-weather-thunderstorm")
    (96 . "nf-weather-thunderstorm") (99 . "nf-weather-thunderstorm")))

(defconst boem-weather--code->desc
  '((0  . "Clear sky") (1  . "Mainly clear") (2  . "Partly cloudy") (3  . "Overcast")
    (45 . "Fog") (48 . "Fog") (51 . "Light drizzle") (53 . "Moderate drizzle")
    (55 . "Heavy drizzle") (56 . "Freezing drizzle") (57 . "Freezing drizzle")
    (61 . "Slight rain") (63 . "Rain") (65 . "Heavy rain")
    (66 . "Light freezing rain") (67 . "Heavy freezing rain")
    (71 . "Slight snow") (73 . "Snow") (75 . "Heavy snow")
    (77 . "Snow grains") (80 . "Rain showers") (81 . "Rain showers")
    (82 . "Violent rain showers") (85 . "Snow showers") (86 . "Snow showers")
    (95 . "Thunderstorm") (96 . "Thunderstorm") (99 . "Thunderstorm")))

(defun boem-weather--code-icon (code)
  (boem-weather--wi (or (alist-get code boem-weather--code->icon) "na")))

(defun boem-weather--code-desc (code)
  (alist-get code boem-weather--code->desc "Unknown"))

(defun boem-weather--wind-icon (deg)
  "Return arrow icon for wind direction DEG."
  (let* ((dir (mod (+ deg 22.5) 360))
         (idx (floor (/ dir 45)))
         )
    (nth idx '("↑" "↗" "→" "↘" "↓" "↙" "←" "↖"))))

;;; ----------------------------------------------------------------
;;; Fetching
;;; ----------------------------------------------------------------

(defun boem-weather--url (compression)
  (let* ((weather-param
          (cond ((eq 'current compression) boem-weather--current-param)
                ((eq 'hourly compression) boem-weather--hourly-param)
                ((eq 'daily compression) boem-weather--daily-param)))
         (all-params (append `(("latitude" ,boem-weather-latitude)
                               ("longitude" ,boem-weather-longitude)
                               ("time_zone" ,(url-hexify-string boem-weather-time-zone))) weather-param)))
    (format "https://api.open-meteo.com/v1/forecast?%s"
            (url-build-query-string all-params))
    ))

(defun boem-weather--fetch (compression)
  (let ((buf (url-retrieve-synchronously (boem-weather--url compression) t t 10)))
    (unless buf (error "Could not fetch weather data"))
    (with-current-buffer buf
      (goto-char (point-min))
      (re-search-forward "\r?\n\r?\n")          ; skip headers
      (setq boem-weather--data
            (json-parse-string
             (buffer-substring-no-properties (point) (point-max))
             :object-type 'hash-table
             :array-type 'list)))
    (kill-buffer buf)))

;;; ----------------------------------------------------------------
;;; Formatting
;;; ----------------------------------------------------------------
(defun boem-weather--fmt-current (data)
  (let* ((current_weather (gethash "current" data))
         (weather_code (gethash "weather_code" current_weather))
         (current_units (gethash "current_units" data)))
    (format
"%s %2d%s (%2d%s)\n\
%s %s\n\
%s %d %s\n\
%s %d%s (%d %s)\n\
%d %s\n\
%s %d %s"
     (boem-weather--wi "nf-weather-thermometer")
     (gethash "temperature_2m" current_weather)
     (gethash "temperature_2m" current_units)
     (gethash "apparent_temperature" current_weather)
     (gethash "temperature_2m" current_units)
     (boem-weather--code-icon weather_code)
     (boem-weather--code-desc weather_code)
     (boem-weather--wind-icon (gethash "wind_direction_10m" current_weather))
     (gethash "wind_speed_10m" current_weather)
     (gethash "wind_speed_10m" current_units)
     (boem-weather--wi "nf-weather-rain")
     (gethash "precipitation_probability" current_weather)
     (gethash "precipitation_probability" current_units)
     (gethash "precipitation" current_weather)
     (gethash "precipitation" current_units)
     (gethash "pressure_msl" current_weather)
     (gethash "pressure_msl" current_units)
     (boem-weather--wi "nf-weather-humidity")
     (gethash "relative_humidity_2m" current_weather)
     (gethash "relative_humidity_2m" current_units)
     )
    )
  )

(defun boem-weather--fmt-daily (data)
  "Format daily weather data into a list of rows, one row per day."
  (let* ((daily (gethash "daily" data))
         (dates (gethash "time" daily))
         (sunrise (gethash "sunrise" daily))
         (sunset (gethash "sunset" daily))
         (temp-min (gethash "temperature_2m_min" daily))
         (temp-max (gethash "temperature_2m_max" daily))
         (wcode (gethash "weather_code" daily))
         (wind-speed (gethash "wind_speed_10m_max" daily))
         (wind-direction (gethash "wind_direction_10m_dominant" daily))
         (precip (gethash "precipitation_probability_max" daily))
         (units (gethash "daily_units" data))
         (temp-unit (gethash "temperature_2m_min" units))
         (precip-unit (gethash "precipitation_probability_max" units))
         (result-lines nil))
    (dotimes (i 7)
      (push (format "%s  %s%s - %s%s   %s %s%2d/%2d%s   %s %s%2d %s  %2d%s"
              (nth i dates)
              (boem-weather--wi "nf-weather-sunrise")
              (substring (nth i sunrise) -5)
              (boem-weather--wi "nf-weather-sunset")
              (substring (nth i sunset) -5)
              (boem-weather--code-icon (nth i wcode))
              (boem-weather--wi "nf-weather-thermometer")
              (nth i temp-min)
              (nth i temp-max)
              temp-unit
              (nerd-icons-mdicon "nf-md-weather_windy")
              (boem-weather--wind-icon (nth i wind-direction))
              (nth i wind-speed)
              (gethash "wind_speed_10m_max" units)
              (nth i precip)
              precip-unit
              ) result-lines)
      )
    (string-join (nreverse result-lines) "\n")))

(defun boem-weather--fmt-hourly (data)
  (let* ((hourly (gethash "hourly" data))
         (times (gethash "time" hourly))
         (temperature (gethash "temperature_2m" hourly))
         (apparent_temperature (gethash "apparent_temperature" hourly))
         (wcode (gethash "weather_code" hourly))
         (wind-speed (gethash "wind_speed_10m" hourly))
         (wind-direction (gethash "wind_direction_10m" hourly))
         (precip (gethash "precipitation_probability" hourly))
         (units (gethash "hourly_units" data))
         (temp-unit (gethash "temperature_2m" units))
         (precip-unit (gethash "precipitation_probability" units))
         (wind-speed-unit (gethash "wind_speed_10m" units))
         (result-lines nil))
    (dotimes (i (length times))
             (push (format "%s  %s %s%2d%s (%2d%s)   %s %s%2d %s  %2d%s"
                           (substring (nth i times) 11 16)
                           (boem-weather--code-icon (nth i wcode))
                           (boem-weather--wi "nf-weather-thermometer")
                           (nth i temperature)
                           temp-unit
                           (nth i apparent_temperature)
                           temp-unit
                           (nerd-icons-mdicon "nf-md-weather_windy")
                           (boem-weather--wind-icon (nth i wind-direction))
                           (nth i wind-speed)
                           (gethash "wind_speed_10m" units)
                           (nth i precip)
                           precip-unit
                           ) result-lines)
             )
    (string-join (nreverse result-lines) "\n")))

(defun boem-weather--fmt-daily-sunrise-line (data)
  (let* ((daily (gethash "daily" data))
         (sunrise (gethash "sunrise" daily))
         (sunset (gethash "sunset" daily))
         )
    (mapconcat
       (lambda (i)
         (format "%s%s - %s%s"
                 (boem-weather--wi "nf-weather-sunrise")
                 (substring (nth i sunrise) -5)
                 (boem-weather--wi "nf-weather-sunset")
                 (substring (nth i sunset) -5))
         )
       (number-sequence 0 (1- (length temp-min))) "  ")))

(defun boem-weather--fmt-daily-temp-line (data)
  (let* ((daily (gethash "daily" data))
         (temp-min (gethash "temperature_2m_min" daily))
         (temp-max (gethash "temperature_2m_max" daily))
         (wcode (gethash "weather_code" daily))
         (unit (gethash "temperature_2m_min" (gethash "daily_units" data)))
         )
      (mapconcat
       (lambda (i)
         (format "%s %d/%d%s"
                 (boem-weather--code-icon (nth i wcode))
                 (nth i temp-min)
                 (nth i temp-max)
                 unit)
         )
       (number-sequence 0 (1- (length temp-min))) "")))

(defun boem-weather--fmt-daily-wind-line (data)
  (let* ((daily (gethash "daily" data))
         (speeds (gethash "wind_speed_10m_max" daily))
         (directions (gethash "wind_direction_10m_dominant" daily))
         )
    (mapconcat
       (lambda (i)
         (format "%s%s"
                 (boem-weather--wind-icon (nth i directions))
                 (nth i speeds))
         )
       (number-sequence 0 (1- (length speeds))) "  ")))

(defun boem-weather--fmt-daily-precipitation-line (data)
  (let* ((daily (gethash "daily" data))
         (precipitations (gethash "precipitation_probability_max" daily))
         (unit (gethash "precipitation_probability_max" (gethash "daily_units" data)))
         )
    (mapconcat
       (lambda (i)
         (format "%s%s" (nth i precipitations) unit))
       (number-sequence 0 (1- (length precipitations))) "  ")))

(defun boem-weather--section-string (compression)
  (boem-weather--fetch compression)
  (pcase compression
    ('current (boem-weather--fmt-current boem-weather--data))
    ('daily   (boem-weather--fmt-daily   boem-weather--data))
    ('hourly  (boem-weather--fmt-hourly  boem-weather--data))))

;;; ----------------------------------------------------------------
;;; Display
;;; ----------------------------------------------------------------

(defun boem-weather--display-in-child-frame (display-buffer)
  "Display buffer in a child frame centered over the parent frame.
The child frame has no decorations except a 2-pixel white border.
Closes on any key press."
  (interactive)
  (let* ((parent-frame (selected-frame))
         (parent-width (frame-pixel-width parent-frame))
         (parent-height (frame-pixel-height parent-frame))
         pos-left pos-top)
    ;; Calculate size of text in lines and columns
    (with-current-buffer display-buffer
      (goto-char (point-min))
      (let* ((max-line-width 0)
             (line-count 0))
        (while (not (eobp))
          (let ((len (length (string-trim-right (thing-at-point 'line t)))))
            (setq max-line-width (max max-line-width len)))
          (forward-line 1)
          (setq line-count (1+ line-count)))
        (let* ((char-width (frame-char-width parent-frame))
               (char-height (frame-char-height parent-frame))
               (frame-pixel-width (* max-line-width char-width))
               (frame-pixel-height (* line-count char-height)))
          (setq pos-left (/ (- parent-width frame-pixel-width) 2))
          (setq pos-top (/ (- parent-height frame-pixel-height) 2))
          (if (not (frame-live-p boem-weather--frame))
            (setq boem-weather--frame
                (make-frame `((parent-frame . ,parent-frame)
                              (minibuffer . nil)
                              (left . ,pos-left)
                              (top . ,pos-top)
                              (width . ,(+ 1 max-line-width))
                              (height . ,line-count)
                              (undecorated . t)
                              (internal-border-width . 3)
                              (no-accept-focus . nil)
                              (no-other-frame . t)
                              (no-special-glyphs . t)
                              (vertical-scroll-bars . nil)
                              (horizontal-scroll-bars . nil)
                              (menu-bar-lines . 0)
                              (tool-bar-lines . 0)
                              (tab-bar-lines .0)
                              (line-spacing . 0)
                              (visibility . nil)
                              (user-position . t)
                              (drag-internal-border . nil)
                              (border-color . "white")
                              (background-color . ,(face-background 'default nil t)))))
            (progn
              (fit-frame-to-buffer boem-weather--frame)
              (let* ((parent (frame-parent boem-weather--frame))
                     (char-width (frame-char-width parent-frame))
                     (char-height (frame-char-height parent-frame))
                     (left-pos (* (/ (- (frame-width parent) (frame-width boem-weather--frame)) 2) char-width))
                     (top-pos (* (/ (- (frame-height parent) (frame-height boem-weather--frame)) 2) char-height)))
                (set-frame-position boem-weather--frame left-pos top-pos)
                )
              ))
          )
        (with-selected-frame boem-weather--frame
          (switch-to-buffer display-buffer)
          (goto-char (point-min))
          (setq-local cursor-type nil)
          (setq-local mode-line-format nil)
          (setq-local header-line-format nil)
          (redisplay)
          (let ((map (make-sparse-keymap)))
            (set-keymap-parent map (current-local-map))
            (define-key map [t]
                        (lambda ()
                          (interactive)
                          (when (frame-live-p boem-weather--frame)
                            (delete-frame boem-weather--frame))
                          (when (buffer-live-p display-buffer)
                            (kill-buffer display-buffer))))
            (use-local-map map))
          (set-frame-parameter nil 'visibility t))))))

(defun boem-weather--show (compression)
  (let* ((buf (get-buffer-create boem-weather--buffer-name))
         (txt (boem-weather--section-string compression)))
    (with-current-buffer buf
      (read-only-mode -1)
      (erase-buffer)
      (insert txt)
      (goto-char (point-min))
      (weather-view-mode)
      (setq mode-line-format nil
            truncate-lines   t)
      (read-only-mode 1)
      (boem-weather--display-in-child-frame buf)
      ))
  )

;;; ----------------------------------------------------------------
;;; Commands / keymaps
;;; ----------------------------------------------------------------
(defun boem-weather-refresh () (interactive) (setq boem-weather--data nil) (boem-weather--show))
(defun boem-weather-quit    () (interactive)
       (when-let* ((buf (get-buffer boem-weather--buffer-name)))
         (dolist (win (get-buffer-window-list buf nil t))
           (when (frame-parameter (window-frame win) 'parent-frame)
             (delete-frame (window-frame win))))
         (kill-buffer buf)))

(defvar weather-view-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "c") (lambda () (interactive) (boem-weather--show 'current)))
    (define-key map (kbd "d") (lambda () (interactive) (boem-weather--show 'daily)))
    (define-key map (kbd "h") (lambda () (interactive) (boem-weather--show 'hourly)))
    (define-key map (kbd "g") #'boem-weather-refresh)
    (define-key map (kbd "q") #'boem-weather-quit)
    map))

(define-derived-mode weather-view-mode special-mode "Weather"
  "Major mode for displaying weather from boem-weather.com.")

;;; autoload
(defun boem-weather ()
  "Show current weather in a child frame."
  (interactive)
  (boem-weather--show 'current))

;; set global key
(define-key global-map boem-weather-keybinding #'boem-weather)

(defun display-in-buffer (txt)
  (with-current-buffer (get-buffer-create "*Transposed Weather Info*")
    (erase-buffer)
    (insert txt)
    (pop-to-buffer (current-buffer))
    ))

(provide 'boem-weather)
;;; boem-weather.el ends here
