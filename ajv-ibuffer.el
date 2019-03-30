(provide 'ajv-ibuffer)

(setq ibuffer-show-empty-filter-groups nil
      ibuffer-default-sorting-mode 'major-mode)

(setq ibuffer-saved-filter-groups
      (quote (("Uncategorized"
	       ("Emacs"
		(or
                 (name . "^\\*scratch\\*$")
                 (name . "^\\*Messages\\*$")))
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

(defun ajv/group-ibuffer-by-vc ()
  (interactive)
  (ibuffer-vc-set-filter-groups-by-vc-root)
  (unless (eq ibuffer-sorting-mode 'alphabetic)
    (ibuffer-do-sort-by-alphabetic)))

(defun ajv/ibuffer-use-default-filter ()
  (interactive)
  (ibuffer-switch-to-saved-filter-groups "Uncategorized"))

;; The next two were picked from here:
;; Wraparound cursor movement: https://www.emacswiki.org/emacs/IbufferMode#toc14
(defun ibuffer-previous-line ()
  "While in ibuffer, previous line (bound to <down>/'p') at the top will wrap around to end of buffer
The hardcoded 2 may need to change if IBuffer config changes"
  (interactive)
  (previous-line)
  (if (<= (line-number-at-pos) 2)
      (goto-line (- (count-lines (point-min) (point-max)) 2))))

(defun ibuffer-next-line ()
  "While in ibuffer, next line (bound to <up>/'n') at the bottom will wrap around to beginnning of buffer.
The hardcoded 3 may need to change if IBuffer config changes"
  (interactive)
  (next-line)
  (if (>= (line-number-at-pos) (- (count-lines (point-min) (point-max)) 1))
      (goto-line 3)))


(defun ajv/human-readable-file-sizes-to-bytes (string)
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

(defun ajv/bytes-to-human-readable-file-sizes (bytes)
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
		     (+ (float (ajv/human-readable-file-sizes-to-bytes string))
			total)))
	     (ajv/bytes-to-human-readable-file-sizes total)))	 ;; :summarizer nil
	 )
  (ajv/bytes-to-human-readable-file-sizes (buffer-size)))

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