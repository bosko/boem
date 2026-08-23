;;; boem-line-jump.el --- Quick line jump with labels  -*- lexical-binding: t; -*-
;;
;; Author: Boško Ivanišević
;; URL: https://github.com/bosko/boem
;; Keywords: convenience
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; Provides a quick line-jump command that labels visible lines
;; with marks, allowing fast jumping.
;;
;;; Code:

(defun boem/list-windows ()
  "Return a list of windows in the current frame, ordered from top to bottom, left to right."
  (sort (window-list nil 'no-mini)
        (lambda (w1 w2)
          (let ((edges1 (window-edges w1))
                (edges2 (window-edges w2)))
            (or (< (car edges1) (car edges2)) ; Compare top edges
                (and (= (car edges1) (car edges2)) ; If equal, compare left edges
                     (< (cadr edges1) (cadr edges2))))))))

(defun boem/display-line-marks (window start-with-mark)
  "Display START-WITH-MARK at the left side of each visible line in WINDOW."
  (with-current-buffer (window-buffer window)
    ;; Remove old marks first.
    (boem/remove-marks window)

    (save-excursion
      (goto-char (window-start window))
      (beginning-of-line)
      (let* ((end (window-end window t)))
        (while (< (point) end)
          (let* ((cur-mark (boem/next-mark start-with-mark))
                 (ov (make-overlay (point) (point) nil t t)))
            (overlay-put ov 'boem-line-mark t)
            (overlay-put ov 'before-string (boem/display-mark cur-mark))
            (setq start-with-mark cur-mark))
          (forward-line 1)))))
  start-with-mark)

(defun boem/remove-marks (window)
  "Remove marks set with boem/display-line-marks from WINDOW."
  (with-current-buffer (window-buffer window)
    "Removes all overlays from the window"
    (remove-overlays (window-start window)
                     (window-end window t)
                     'boem-line-mark t)))

(defun boem/clear-all-marks ()
  "Removes overlays from all windows."
  (dolist (win (boem/list-windows))
    (boem/remove-marks win)))

(defun boem/next-mark (cur-mark)
  "Return the next lowercase mark string after CUR-MARK.

If CUR-MARK is nil or empty, return \"a\".
Sequence is base-26 using letters a-z."
  (let ((s (if (or (not (stringp cur-mark)) (string-empty-p cur-mark))
               ;; This will be increased by one later giving
               ;; 'a' as a result
               "`"
             (string-trim cur-mark))))
    (if (string-match-p "\\`z+\\'" s)
        (concat "a" (make-string (length s) ?a))
      (let* ((chars (string-to-list s))
             (i (1- (length s))))
        (while (and (>= i 0) (= (nth i chars) ?z))
          (setf (nth i chars) ?a)
          (setq i (1- i)))

        (when (>= i 0)
          (setf (nth i chars) (1+ (nth i chars))))

        (apply #'string chars)))))

(defun boem/highlight-marks (window to-highlight)
  "Highlight all marks in WINDOW which start with TO-HIGHLIGHT."
  (with-current-buffer (window-buffer window)
    (let*
        ((overlays (car (overlay-lists)))
         (i (1- (length overlays))))
      (while (>= i 0)
        (let* ((cur-mark
                (string-trim
                 (substring-no-properties
                  (or (overlay-get (nth i overlays) 'before-string) "")))))
          (if (> (length cur-mark) 0)
              (overlay-put (nth i overlays) 'before-string
                           (boem/display-mark cur-mark to-highlight))))
        (setq i (1- i))))))

(defun boem/display-mark (mark &optional highlight-string)
  "Formats MARK for displaying in buffer.
If HIGHLIGHT-STRING is given it highlights it."
  (let* ((formatted (format "%4s " mark))
         (to-display (propertize formatted 'face 'fringe)))
    (if (and (stringp highlight-string)
             (> (length highlight-string) 0)
             (string-prefix-p highlight-string mark))
        (let ((idx (string-match-p highlight-string formatted)))
          (if idx
              (add-text-properties
               idx (+ idx (length highlight-string))
               '(face compilation-error) to-display))
          to-display)
      to-display)))

(defun boem/find-overlay (mark)
  "Searches all windows and returns overlay with MARK at the beginning."
  (let ((target-overlay))
    (dolist (win (boem/list-windows))
      (with-current-buffer (window-buffer win)
        (let* ((overlays (car (overlay-lists)))
               (overlays
                (seq-filter (lambda (o) (overlay-get o 'boem-line-mark)) overlays))
               (i (1- (length overlays))))
          (while (>= i 0)
            (let* ((cur-mark
                    (string-trim
                     (substring-no-properties
                      (or (overlay-get (nth i overlays) 'before-string) "")))))
              (if (string= mark cur-mark)
                  (setq target-overlay (nth i overlays))))
            (setq i (1- i))))))
    target-overlay))

(defun boem/jump-to-line ()
  "Jumps to the selected line."
  (interactive)
  (let ((captured)
        (start-mark)
        (wins (boem/list-windows)))
    (dolist (win wins) (setq start-mark (boem/display-line-marks win start-mark)))
    (minibuffer-with-setup-hook
        (lambda ()
          (add-hook
           'post-command-hook
           (lambda ()
             (setq captured (minibuffer-contents-no-properties))
             (dolist (win1 wins) (boem/highlight-marks win1 captured)))
           nil t))
      (read-from-minibuffer "Jump to line: "))
    (let ((overlay (boem/find-overlay captured)))
      (if (overlayp overlay)
          (progn
            (select-window (get-buffer-window (overlay-buffer overlay)))
            (goto-char (overlay-start overlay))))))
  (boem/clear-all-marks))

(provide 'boem-line-jump)
;;; boem-line-jump.el ends here
