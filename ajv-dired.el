(provide 'ajv-dired)

(defun ajv/dired-sort-criteria (criteria)
  "sort-dired by different criteria by Robert Gloeckner, 
later modified by Akshar Varma"
  (interactive 
   (list (or (ido-completing-read "criteria [name]: "
                                  '("size(S)" "extension(X)" "creation-time(ct)"
                                    "access-time(ut)" "time(t)" "name(n)"))
             "")))
  (string-match ".*(\\(.*\\))" criteria)
  (dired-sort-other (concat dired-listing-switches " -"
                            (match-string 1 criteria))))


(defun ajv/dired-hide-details-omit-hidden-files ()
    "Hide details and omit hidden files in dired mode"
  (interactive)
  (dired-hide-details-mode)             ;hide details due to 'ls -l'
  (use-package dired-x
    :config                             ;omit hidden files
      (setq dired-omit-files "^\\...+$")
      (dired-omit-mode 1))
  )

(defun ajv/dired-set-default-sorting ()
  "Set default sorting criteria for directories in ajv/dired-default-sorting-alist"
  (interactive)
  (if (assoc-default default-directory ajv/dired-default-sorting-alist)
      (ajv/dired-sort-criteria (assoc-default default-directory ajv/dired-default-sorting-alist))
    nil))


