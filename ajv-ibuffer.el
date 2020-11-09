(provide 'ajv-ibuffer)

(setq ibuffer-show-empty-filter-groups nil
      ibuffer-default-sorting-mode 'major-mode
      ibuffer-expert t)

(setq ibuffer-saved-filter-groups
      (quote (("Uncategorized"
	       ("Emacs"
		(or
                 (name . "^\\*scratch\\*$")
                 (name . "^\\*Messages\\*$")))
	       ("Music"
		(name . "^\\*ajv-mpv-buffer\\*$"))
	       ("Org" ;; all org-related buffers
		(or
		 (mode . org-mode)
		 (name . "^\\*Org Agenda\\*$")))
	       ("Mail"
		(or  ;; mail-related buffers
		 (mode . message-mode)
		 (mode . mail-mode)))
	       ("Notmuch"
		(or
		 (mode . notmuch-search-mode)
		 (mode . notmuch-hello-mode)
		 (mode . notmuch-show-mode)))
	       ("Dired"
		(mode . dired-mode))
	       ("LaTeX"
		(mode . LaTeX-mode))
	       ("PDFs"
		(mode . pdf-view-mode))
	       ("Programming"
		(or
                 (mode . ess-mode)
                 (mode . python-mode)
                 (mode . emacs-lisp-mode)))
	       ("Internals"
		(name . "^\\*.*\\*$"))
	       ))))

(defun ajv/ibuffer/group-by-vc ()
  (interactive)
  (ibuffer-vc-set-filter-groups-by-vc-root)
  (unless (eq ibuffer-sorting-mode 'alphabetic)
    (ibuffer-do-sort-by-alphabetic)))

(defun ajv/ibuffer/use-default-filter ()
  (interactive)
  (ibuffer-switch-to-saved-filter-groups "Uncategorized"))

;; The next two were picked from here:
;; Wraparound cursor movement: https://www.emacswiki.org/emacs/IbufferMode#toc14
(defun ajv/ibuffer/previous-line ()
  "While in ibuffer, previous line (bound to <down>/'p') at the top will wrap around to end of buffer
The hardcoded 2 may need to change if IBuffer config changes"
  (interactive)
  (previous-line)
  (if (<= (line-number-at-pos) 2)
      (goto-line (- (count-lines (point-min) (point-max)) 2))))

(defun ajv/ibuffer/next-line ()
  "While in ibuffer, next line (bound to <up>/'n') at the bottom will wrap around to beginnning of buffer.
The hardcoded 3 may need to change if IBuffer config changes"
  (interactive)
  (next-line)
  (if (>= (line-number-at-pos) (- (count-lines (point-min) (point-max)) 1))
      (goto-line 3)))


(defun ajv/ibuffer/human-readable-file-sizes-to-bytes (string)
  "Convert a human-readable file size into bytes."
  (interactive)
  (cond
   ((string-suffix-p "G" string t)
    (* 1000000000 (string-to-number (substring string 0 (- (length string) 1)))))
   ((string-suffix-p "M" string t)
    (* 1000000 (string-to-number (substring string 0 (- (length string) 1)))))
   ((string-suffix-p "K" string t)
    (* 1000 (string-to-number (substring string 0 (- (length string) 1)))))
   (t
    (string-to-number (substring string 0 (- (length string) 1))))
   )
  )

(defun ajv/ibuffer/bytes-to-human-readable-file-sizes (bytes)
  "Convert number of bytes to human-readable file size."
  (interactive)
  (cond
   ((> bytes 1000000000) (format "%10.1fG" (/ bytes 1000000000.0)))
   ((> bytes 100000000) (format "%10.0fM" (/ bytes 1000000.0)))
   ((> bytes 1000000) (format "%10.1fM" (/ bytes 1000000.0)))
   ((> bytes 100000) (format "%10.0fk" (/ bytes 1000.0)))
   ((> bytes 1000) (format "%10.1fk" (/ bytes 1000.0)))
   (t (format "%10d" bytes)))
  )

;; Use human readable Size column instead of original one
(define-ibuffer-column size-h
  (:name "Size"
	 :inline t
	 :summarizer
	 (lambda (column-strings)
	   (let ((total 0))
	     (dolist (string column-strings)
	       (setq total
		     ;; like, ewww ...
		     (+ (float (ajv/ibuffer/human-readable-file-sizes-to-bytes string))
			total)))
	     (ajv/ibuffer/bytes-to-human-readable-file-sizes total)))	 ;; :summarizer nil
	 )
  (ajv/ibuffer/bytes-to-human-readable-file-sizes (buffer-size)))

;; Modify the default ibuffer-formats
(setq ibuffer-formats
      '((mark modified read-only locked " "
	      (name 20 20 :left :elide)
	      " "
	      (size-h 11 -1 :right)
	      " "
	      (mode 16 16 :left :elide)
	      " "
	      filename-and-process)
	(mark " "
	      (name 16 -1)
	      " " filename)))


(defun ajv/ibuffer/collapse-all-filter-groups ()
  "Collapse all filter groups at once

Taken from: https://acidwords.com/posts/2016-06-18-collapsing-all-filter-groups-in-ibuffer.html"
  (interactive)
  (setq ibuffer-hidden-filter-groups
        (mapcar #'car (ibuffer-current-filter-groups-with-position)))
  (ibuffer-update nil t))

(defun ajv/ibuffer/go-to-beginning-of-buffer ()
  "When in dired-mode, go first file instead of top of buffer.

Taken from: http://whattheemacsd.com/setup-dired.el-02.html"
  (interactive)
  (beginning-of-buffer)
  (dired-next-line 2))

(defun ajv/ibuffer/go-to-end-of-buffer ()
  "When in dired-mode, go to last file instead of end of buffer.

Taken from: http://whattheemacsd.com/setup-dired.el-02.html"
  (interactive)
  (end-of-buffer)
  (dired-next-line -2))
