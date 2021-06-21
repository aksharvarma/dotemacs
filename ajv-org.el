(provide 'ajv-org)

(setq org-startup-truncated 'nil)
(setq org-catch-invisible-edits 'smart)
(setq org-special-ctrl-a/e t)
(setq org-link-search-must-match-exact-headline nil)
(setq org-goto-interface 'outline-path-completion)
(setq org-outline-path-complete-in-steps nil)
(setq org-hide-emphasis-markers t)
(setq org-highlight-latex-and-related '(native))
;; consider changing to 'note so that you add a note when finishing a task
;; See org-log-note-headings for more ideas
(setq org-log-done 'time)
(setq org-log-into-drawer t)
(setq org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "WAITING(w@)"
				    "ONGOING(o!/!)" "MEETING(m)" "INACTIVE(i!/!)"
				    "|" "DONE(d)" "CANCELLED(c)")))
(setq org-todo-keyword-faces
      '(("TODO" . (:inherit bold :foreground "orange"))
	("NEXT" . (:inherit bold :foreground "DeepSkyBlue"))
	("WAITING" . (:inherit bold :foreground "firebrick"))
	("ONGOING" . (:inherit bold-italic :foreground "yellow"))
	("MEETING" . (:inherit bold :foreground "DarkGreen"))
	("INACTIVE" . (:inherit bold :foreground "gray" ))
	("CANCELLED" . shadow)))
(setq org-use-fast-todo-selection 'expert)

(setq org-enforce-todo-dependencies t)
(setq org-enforce-todo-checkbox-dependencies t)
(setq org-agenda-dim-blocked-tasks t)

(message "Messaging: before setq")
(setq org-agenda-files ajv/settings/my-org-agenda-files)
;; (setq org-directory ajv/settings/my-org-agenda-files)
(message "Messaging: after setq")

(setq org-agenda-skip-scheduled-if-done t)
(setq org-habit-show-habits-only-for-today nil)
(setq org-agenda-show-future-repeats 'next)
(setq org-agenda-start-on-weekday nil)
(setq org-agenda-start-day "-1d")
(setq org-agenda-start-with-clockreport-mode t)
(setq org-agenda-start-with-log-mode t)
;; org-agenda-span 6

(setq org-agenda-window-setup 'current-window)

(setq org-agenda-time-grid
      (quote
       ((daily today require-timed remove-match)
	nil
	"..." "-----------")))

(setq safe-local-variable-values '((eval org-shifttab 2)))

;; Allow executing Lisp and Python in org-mode src blocks
;; (org-babel-do-load-languages 'org-babel-load-languages '((emacs-lisp . t) (python 3 . t)))

;; (add-to-list 'org-agenda-custom-commands
;;              '("l" "Show agenda with log and time report"
;; 	       ((agenda "" (org-agenda-clockreport-mode))
;; 		(org-agenda-log-mode))))
