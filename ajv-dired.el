(provide 'ajv-dired)

(defun ajv/dired/sort-criteria (criteria)
  "sort-dired by different criteria by Robert Gloeckner,
later modified by Akshar Varma"
  (interactive
   (list (or (ido-completing-read "criteria [name]: "
                                  '("size(S)" "extension(X)" "creation-time(ct)"
                                    "access-time(ut)" "time(t)" "name(n)"))
             "")))
  (when (eq major-mode 'dired-mode)
    (string-match ".*(\\(.*\\))" criteria)
    (dired-sort-other (concat dired-listing-switches " -"
			      (match-string 1 criteria)))))


(defun ajv/dired/hide-details-omit-hidden-files ()
  "Hide details and omit hidden files in dired mode"
  (interactive)
  (when (eq major-mode 'dired-mode)

    (dired-hide-details-mode)             ;hide details due to 'ls -l'
    (use-package dired-x
      :config                             ;omit hidden files
      (setq dired-omit-files "^\\...+$")
      (dired-omit-mode 1))))

(defun ajv/dired/set-default-sorting ()
  "Set default sorting criteria for directories in ajv/settings/dired-default-sorting-alist"
  (interactive)
  (when (eq major-mode 'dired-mode)
    ;; If one of the directories in ajv/settings/dired-default-sorting-alist,
    ;; then sort accordingly
    (if (assoc-default default-directory
		       ajv/settings/dired-default-sorting-alist
		       'file-equal-p)
	(ajv/dired/sort-criteria
	 (file-name-as-directory (assoc-default default-directory
						ajv/settings/dired-default-sorting-alist
						'file-equal-p)))
      nil)
    ;; If symlink-folder, then don't display . and .. in dired
    (if (file-equal-p default-directory ajv/settings/symlink-folder)
	(dired-sort-other "-A -l -L -h --group-directories-first --classify")
      nil)))


(defun ajv/dired/launch-file ()
  "Launch system associated program on current file in dired buffer
modified from http://omniorthogonal.blogspot.in/2008/05/useful-emacs-dired-launch-hack.html"
  (interactive)
  (when (eq major-mode 'dired-mode)
    (let ((process-connection-type nil))
      (start-process "*launch*" nil "xdg-open" (dired-get-filename)))))

(defun ajv/dired/delete-backup-files ()
  "Delete all backup files in the current dired folder"
  (interactive)
  (when (eq major-mode 'dired-mode)
    (dired-omit-mode 0)
    (dired-unmark-all-marks)
    (dired-flag-backup-files)
    (dired-do-flagged-delete)))

(defun ajv/dired/go-to-beginning-of-buffer ()
  "When in dired-mode, go first file instead of top of buffer.

Taken from: http://whattheemacsd.com/setup-dired.el-02.html"
  (interactive)
  (beginning-of-buffer)
  (dired-next-line 3))

(defun ajv/dired/go-to-end-of-buffer ()
  "When in dired-mode, go to last file instead of end of buffer.

Taken from: http://whattheemacsd.com/setup-dired.el-02.html"
  (interactive)
  (end-of-buffer)
  (dired-next-line -1))

(defun ajv/dired/copy-directory-name-as-kill ()
  "When in dired-mode, copy the name of the directory from the first line in the buffer."
  (interactive)
  (save-excursion (beginning-of-buffer)
		  (dired-copy-filename-as-kill)))
