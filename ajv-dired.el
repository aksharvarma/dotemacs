(provide 'ajv-dired)

(defun ajv/dired-sort-criteria (criteria)
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


(defun ajv/dired-hide-details-omit-hidden-files ()
  "Hide details and omit hidden files in dired mode"
  (interactive)
  (when (eq major-mode 'dired-mode)

    (dired-hide-details-mode)             ;hide details due to 'ls -l'
    (use-package dired-x
      :config                             ;omit hidden files
      (setq dired-omit-files "^\\...+$")
      (dired-omit-mode 1))))

(defun ajv/dired-set-default-sorting ()
  "Set default sorting criteria for directories in ajv/dired-default-sorting-alist"
  (interactive)
  (when (eq major-mode 'dired-mode)
    ;; If one of the directories in ajv/dired-default-sorting-alist,
    ;; then sort accordingly
    (if (assoc-default default-directory
		       ajv/dired-default-sorting-alist
		       'file-equal-p)
	(ajv/dired-sort-criteria
	 (file-name-as-directory (assoc-default default-directory
						ajv/dired-default-sorting-alist
						'file-equal-p)))
      nil)
    ;; If symlink-folder, then don't display . and .. in dired
    (if (file-equal-p default-directory ajv/symlink-folder)
	(dired-sort-other "-A -l -L -h --group-directories-first --classify")
      nil)))


(defun ajv/dired-launch-file ()
  "Launch system associated program on current file in dired buffer
modified from http://omniorthogonal.blogspot.in/2008/05/useful-emacs-dired-launch-hack.html"
  (interactive)
  (when (eq major-mode 'dired-mode)
    (case system-type
      (gnu/linux (let ((process-connection-type nil))
		   (start-process "*launch*" nil "xdg-open" (dired-get-filename))))
      (windows-nt (w32-shell-execute "open"  (dired-get-filename) nil nil)))))

(defun ajv/delete-backup-files ()
  "Delete all backup files in the current dired folder"
  (interactive)
  (when (eq major-mode 'dired-mode)
    (dired-omit-mode 0)
    (dired-unmark-all-marks)
    (dired-flag-backup-files)
    (dired-do-flagged-delete)))

;; TODO: Make sure these are only callable from within dired.
(defun ajv/mpv-marked ()
  "Open marked files as single playlist in mpv"
  (interactive)
  (when (eq major-mode 'dired-mode)
    (dired-do-shell-command "mpv --shuffle --loop-playlist --quiet --force-window 2>&1 1>/dev/null * &" nil (dired-get-marked-files))))

(defun ajv/mpv-all ()
  "Open all files in the dired buffer as single playlist in mpv"
  (interactive)
  (when (eq major-mode 'dired-mode)
    (dired-unmark-all-marks)
    (dired-toggle-marks)
    (ajv/mpv-marked)
    (dired-unmark-all-marks)))

;; ;; TODO: Change this to a function that can then be added as advice around both mpv functions
;; (defadvice ajv/mpv-all (around stfu compile activate)
;;   "Make sure that ajv/mpv-all doesn't ask confirmation before opening new buffer if something is already using the default buffer"
;;   (cl-flet ((yes-or-no-p (&rest args) t)
;; 	 (y-or-n-p (&rest args) t))
;;     ad-do-it))
