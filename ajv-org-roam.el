(provide 'ajv-org-roam)

;; (setq-default org-roam-directory ajv/sensitive/my-org-roam-directory)
(setq-default org-roam-completion-everywhere t)

(setq ajv/org-roam/filename-format "%<%Y%m%d%H%M%S>-${slug}.org")
(setq ajv/org-roam/file-export-options-at-top  "#+TITLE: ${title}\n#+AUTHOR: %n\n#+DATE: %u")

;; (setq ajv/org-roam/capture-file-strings
;;       ("Book" . (concat org-roam-directory "templates/book-roam-template.org")))
(setq org-roam-capture-templates
      `(("d" "Default" plain "# \n# \n%?"
	 :if-new (file+head
		  ,(concat "main/" ajv/org-roam/filename-format)
		  ,ajv/org-roam/file-export-options-at-top)
	 :unnarrowed t)
	;; BOOK
	("b" "Book" plain
	 (file
	  ,(concat org-roam-directory "templates/book-roam-template.org"))
	 :if-new (file+head
		  ,(concat "books/" ajv/org-roam/filename-format)
		  ,(concat ajv/org-roam/file-export-options-at-top
			   "\n#+FILETAGS: :book:"))
	 :unnarrowed t)
	;; MOVIE
	("m" "Movie" plain
	 (file
	  ,(concat org-roam-directory "templates/movie-roam-template.org"))
	 :if-new (file+head
		  ,(concat "movies/" ajv/org-roam/filename-format)
		  ,(concat ajv/org-roam/file-export-options-at-top
			   "\n#+FILETAGS: :movie:"))
	 :unnarrowed t)
	;; SHOW/SERIES
	("s" "Show/Series" plain
	 (file
	  ,(concat org-roam-directory "templates/show-series-roam-template.org"))
	 :if-new (file+head
		  ,(concat "show-series/" ajv/org-roam/filename-format)
		  ,(concat ajv/org-roam/file-export-options-at-top
			   "\n#+FILETAGS: :show-series:"))
	 :unnarrowed t)
	;; MISCELLANEOUS WATCHING
	("v" "Miscellaneous Watching" plain
	 (file
	  ,(concat org-roam-directory "templates/miscellaneous-watching-roam-template.org"))
	 :if-new (file+head
		  ,(concat "misc-watching/" ajv/org-roam/filename-format)
		  ,(concat ajv/org-roam/file-export-options-at-top
			   "\n#+FILETAGS: :misc-watching:"))
	 :unnarrowed t)
	;; LINKS
	("l" "Links" plain
	 (file
	  ,(concat org-roam-directory "templates/links-roam-template.org"))
	 :if-new (file+head
		  ,(concat "links/" ajv/org-roam/filename-format)
		  ,(concat ajv/org-roam/file-export-options-at-top
			   "\n#+FILETAGS: :link:"))
	 :unnarrowed t)
	;; TECH-TIPS
	("t" "Tech Tips" plain
	 (file
	  ,(concat org-roam-directory "templates/tech-tips-roam-template.org"))
	 :if-new (file+head
		  ,(concat "tech-tips/" ajv/org-roam/filename-format)
		  ,(concat ajv/org-roam/file-export-options-at-top
			   "\n#+FILETAGS: :tech-tips:"))
	 :unnarrowed t)
	("r" "Reference (Bibliographical)" plain
	 (file
	  ,(concat org-roam-directory "templates/reference-roam-template.org"))
	 :if-new (file+head
		  ,(concat "references/" ajv/org-roam/filename-format)
		  ,(concat ajv/org-roam/file-export-options-at-top
			   "\n#+FILETAGS: :reference:"))
	 :unnarrowed t)
	("i" "Information (Generic)" plain
	 (file ,(concat org-roam-directory "templates/information-roam-template.org"))
	 :if-new (file+head
		  ,(concat "information/" ajv/org-roam/filename-format)
		  ,(concat ajv/org-roam/file-export-options-at-top
			   "\n#+FILETAGS: :information:"))
	 :unnarrowed t)
	("p" "Person" plain
	 (file ,(concat org-roam-directory "templates/person-roam-template.org"))
	 :if-new (file+head
		  ,(concat "people/" ajv/org-roam/filename-format)
		  ,(concat ajv/org-roam/file-export-options-at-top
			   "\n#+FILETAGS: :person:"))
	 :unnarrowed t
	 :time-prompt t)
	))


(cl-defmethod org-roam-node-type ((node org-roam-node))
  "Return the TYPE of NODE."
  (condition-case nil
      (file-name-nondirectory
       (directory-file-name
        (file-name-directory
         (file-relative-name (org-roam-node-file node) org-roam-directory))))
    (error "")))

(defun ajv/org-roam/close-everything ()
  (interactive)
  (save-excursion
    (let ((count 0))
      (dolist (buffer (buffer-list))
        (set-buffer buffer)
        (when (equal org-roam-directory
		     (file-name-directory (expand-file-name (buffer-name buffer))))
          (setq count (1+ count))
          (kill-buffer buffer)))
      (message "Killed %i org-roam file(s)." count))))

(defun ajv/org-roam/calc-person-current-age ()
  (interactive)
  (goto-char (point-min))
  (let ((age (/ (abs (org-time-stamp-to-now
		      (progn (search-forward "Birthdate:")
			     (search-forward "[")
			     (set-mark-command nil)
			     (org-end-of-line)
			     (backward-char)
			     (buffer-substring (mark) (point)))))
		365.0)))
    (search-forward "Current Age:")
    (search-forward " ")
    (set-mark-command nil)
    (org-end-of-line)
    (delete-active-region)
    (insert (format "%.2f" age)))
  (save-buffer))
