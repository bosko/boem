(require 'org-table)
(require 's)

(defvar voc-words-list () "All words from vocabulary")
(defvar voc-chosen-words () "Words chosen to be learned in one chunk")
(defvar voc-round-no 0 "Round number")
(defvar voc-correct-answers 0 "Number of correct answers")
(defvar voc-wrong-answers 0 "Number of wrong answers")
(defvar voc-run t)
(defvar voc-chunk-size 10 "Number of words that will be learnd in one chunk")
(defvar voc-reapeat-cnt 7 "Number of repeats of learning for each chunk")

(defun voc-load-words-table ()
  (setq voc-words-list (org-table-to-lisp))
  ; Remove heading...
  (setq voc-words-list (cdr voc-words-list))
  ; ... and horizontal line below it.
  (setq voc-words-list (cdr voc-words-list)))

(defun voc-report ()
  (goto-char 0)
  (insert (concat (propertize (format "Krug broj %s\n" voc-round-no) 'face '(:foreground "blue" :height 1.5 :weight bold))
                  (propertize (format "Tačnih pogodaka: %s\n" voc-correct-answers) 'face '(:foreground "green" :height 1.5 :weight extra-bold))
                  (propertize (format "Netačnih pogodaka: %s\n" voc-wrong-answers) 'face '(:foreground "red" :height 1.5 :weight ultra-bold))))
  (insert "---------------------------------\n")
  )

(defun voc-print (string)
  (if (stringp string)
      (insert string)
    (insert (prin1-to-string string))))

(defun voc-read-line ()
  (let (line)
    (setq line (read-string ""))
    (insert line) line))

(defun choose-words ()
  (setq voc-chosen-words ())
  (let ((rnd-index -1) (used-indices ()))
    (while (< (length voc-chosen-words) voc-chunk-size)
           (setq rnd-index (random (- (length voc-words-list) 1)))
           (if (not (member rnd-index 'used-indices))
               (progn
                 (add-to-list 'used-indices rnd-index)
                 (add-to-list 'voc-chosen-words (nth rnd-index voc-words-list)))
             )
           )
    )
  )

(defun voc-check-translation (translation)
  )

(defun voc-main-loop ()
  (while voc-run
    )
  )

(defun vocabulary-mode ()
  (interactive)
  (if (not (org-at-table-p))
      (message "Not inside vocabulary table")
    (progn
      (voc-load-words-table)
      (switch-to-buffer "*Vocabulary*")
      (voc-report)
      (choose-words)
      (setq line (downcase (voc-read-line)))
      (voc-check-translation line))))
