(provide 'ajv-nov)

(defun ajv/nov/font-setup ()
  (face-remap-add-relative 'variable-pitch :family "Liberation Sans"
                           :height 1.25))

(defvar ajv/nov/mode-modeline-format)
(defun ajv/nov/save-disable-modeline-format ()
  "Removes mode-line when in nov-mode. Use toggle function to show/hide."
  (interactive)
  (setq ajv/nov/mode-modeline-format mode-line-format)
  (setq mode-line-format nil))

(defun ajv/nov/toggle-modeline ()
  "Toggles displaying the mode-line when in nov-mode."
  (interactive)
  (if mode-line-format
      (setq mode-line-format nil)
    (setq mode-line-format ajv/nov/mode-modeline-format))
  ;; Following line needed to actually see the change
  (force-mode-line-update 1))

(defun ajv/nov/margin-and-width-adjustments ()
  (let ((total (window-width))
	(left-margin (floor (/ (window-width) 3)))
	(text-width (floor (/ (window-width) 2))))
    (when (eq major-mode "EPUB")
      (setq nov-text-width text-width)
      (set-window-margins nil left-margin))))

(defun ajv/nov/window-configuration-change-hook-fn ()
  (ajv/nov/post-html-render-margin-adjustments)
  (remove-hook 'window-configuration-change-hook
	       'ajv/nov/window-configuration-change-hook-fn
	       t))

(defun ajv/nov/post-html-render-margin-adjustments ()
  (let ((total (window-width))
	(left-margin (floor (/ (window-width) 4)))
	(text-width (floor (/ (window-width) 2))))
    (setq nov-text-width text-width)
    (set-window-margins nil left-margin))
  (add-hook 'window-configuration-change-hook
            'ajv/nov/window-configuration-change-hook-fn
            nil t))

(defun ajv/nov/dedicated-frame-on-mode-start ()
  (interactive)
  (let* ((buffername (buffer-name))
	 (orig-margins (window-margins))
	 (old-left-margins (if orig-margins
			       (car orig-margins)
			     0))
	 (window-to-redisplay (selected-window)))
    (bury-buffer)
    (switch-to-buffer-other-frame (buffer-name))
    ;; (make-frame-command)
    (other-frame -1)
    ;; (bury-buffer)
    ;; (other-frame 1)
    ;; (switch-to-buffer-other-frame (buffer-name))
    ;; (other-frame -1)
    (set-window-margins nil old-left-margins)
    (force-window-update window-to-redisplay)
    (redisplay)
    (other-frame 1)
    )
  ;; (let ((orig-frame (selected-frame)))
  ;;   (make-frame-command)
  ;;   (switch-to-buffer-other-frame (buffer-name)))
  )

(defun ajv/nov/quit ()
  (interactive)
  (ajv/kill-this-buffer)
  (delete-frame))


(defun ajv/nov/get-word-count-in-epub-chapter (&optional skip-printing-message)
  "Counts the number of lines, words and character in the chapter. In nov terms, in a `document'."
  (interactive)
  (let ((word-count-to-point (count-words-region (point-min) (point)))
	(word-count-in-chapter (count-words-region (point-min) (point-max))))
    (unless skip-printing-message
      (message (format "Chapter Progress Percentage: %.2f (%d/%d)"
		       word-count-to-point word-count-in-chapter
		       (* 100 (/ (float word-count-to-point) word-count-in-chapter)))))
    (cons word-count-to-point word-count-in-chapter)))

(defun ajv/nov/get-word-count-in-epub-book (&optional skip-printing-message)
  "Probably hacky way to loop through each chapter/`document' and count the words in it."
  (interactive)
  (when (eq major-mode 'nov-mode)
    (setq-local ajv/nov/total-word-count 0)
    (let* ((i 0)
	   (current-chapter-index nov-documents-index)
	   (current-point-index (point))
	   (read-until-index (- current-chapter-index 1))
	   (last-chapter-index (length nov-documents))
	   ;; Count words in this chapter and until point to which read.
	   (chapter-counts (ajv/nov/get-word-count-in-epub-chapter t))
	   ;; Separate into two
	   (word-count-to-point (car chapter-counts))
	   (word-count-in-chapter (cdr chapter-counts))
	   ;; Set these to the (current) total counts appropiately
	   (ajv/nov/total-word-count word-count-in-chapter)
	   (total-word-count-to-point word-count-to-point)
	   )
      ;; Everything that has been read
      (while (< i read-until-index)
	(nov-goto-document i)
	(setq chapter-word-count (cdr (ajv/nov/get-word-count-in-epub-chapter t)))
	(setq total-word-count-to-point
	      (+ total-word-count-to-point chapter-word-count))
	(setq ajv/nov/total-word-count
	      (+ ajv/nov/total-word-count chapter-word-count))
	(setq i (+ 1 i)))
      ;; Skip to chapter not yet started
      (setq i (+ 1 current-chapter-index))
      ;; Count from there until end of book
      (while (< i last-chapter-index)
	(nov-goto-document i)
	(setq ajv/nov/total-word-count
	      (+ ajv/nov/total-word-count
		 (cdr (ajv/nov/get-word-count-in-epub-chapter t))))
	(setq i (+ 1 i)))
      ;; Unless explicitly told to skip printing, print.
      (unless skip-printing-message
	(message (format "Book Progress Percentage: %.2f (%d/%d)"
			 (* 100 (/ (float total-word-count-to-point)
				   ajv/nov/total-word-count))
			 total-word-count-to-point ajv/nov/total-word-count)))
      (nov-goto-document current-chapter-index)
      (goto-char current-point-index)
      ajv/nov/total-word-count)))
