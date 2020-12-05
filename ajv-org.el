(provide 'ajv-org)

(setq org-startup-truncated 'nil
      org-catch-invisible-edits 'smart
      org-log-into-drawer t
      org-special-ctrl-a/e t
      org-link-search-must-match-exact-headline nil
      org-goto-interface 'outline-path-completion
      org-outline-path-complete-in-steps nil
      org-hide-emphasis-markers t
      ;; consider changing to 'note so that you add a note when finishing a task
      ;; See org-log-note-headings for more ideas
      org-log-done 'time
      org-highlight-latex-and-related '(native)
      org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "WAITING(w@)"
				    "ONGOING(o!/!)" "MEETING(m)" "INACTIVE(i!/!)"
				    "|" "DONE(d)" "CANCELLED(c)"))
      org-todo-keyword-faces
      '(("TODO" . (:inherit bold :foreground "orange"))
	("NEXT" . (:inherit bold :foreground "DeepSkyBlue"))
	("WAITING" . (:inherit bold :foreground "firebrick"))
	("ONGOING" . (:inherit bold-italic :foreground "yellow"))
	("MEETING" . (:inherit bold :foreground "DarkGreen"))
	("INACTIVE" . (:inherit bold :foreground "gray" ))
	("CANCELLED" . shadow))
      org-use-fast-todo-selection 'expert)

(setq org-enforce-todo-dependencies t
      org-enforce-todo-checkbox-dependencies t
      org-agenda-dim-blocked-tasks t)

(setq org-agenda-skip-scheduled-if-done t
      org-agenda-files ajv/settings/my-org-agenda-files
      org-habit-show-habits-only-for-today nil
      org-agenda-show-future-repeats 'next
      org-agenda-start-on-weekday nil
      org-agenda-start-day "-1d"
      org-agenda-start-with-clockreport-mode t
      ;; org-agenda-span 6
      )

(setq org-agenda-window-setup 'current-window)

(setq org-agenda-time-grid
      (quote
       ((daily today require-timed remove-match)
	nil
	"......" "----------------")))

(setq safe-local-variable-values '((eval org-shifttab 2)))

;; Allow executing Lisp and Python in org-mode src blocks
;; (org-babel-do-load-languages 'org-babel-load-languages '((emacs-lisp . t) (python 3 . t)))

;; (add-to-list 'org-agenda-custom-commands
;;              '("l" "Show agenda with log and time report"
;; 	       ((agenda "" (org-agenda-clockreport-mode))
;; 		(org-agenda-log-mode))))
