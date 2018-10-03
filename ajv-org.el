(setq org-startup-truncated 'nil
      org-todo-keywords '((sequence "TODO(t)" "|" "IN-PROGRESS(i)" "|" "DONE(d)"))
      org-agenda-skip-scheduled-if-done t
      org-agenda-files ajv/my-org-agenda-files)

(setq org-agenda-time-grid
      (quote
       ((daily today require-timed remove-match)
	nil
	"......" "----------------")))
