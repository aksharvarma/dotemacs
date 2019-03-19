(provide 'ajv-org)

(setq org-startup-truncated 'nil
      org-todo-keywords '((sequence "TODO(t)" "IN-PROGRESS(i)" "|" "DONE(d)")))

(setq org-agenda-skip-scheduled-if-done t
      org-agenda-files ajv/my-org-agenda-files
      org-habit-show-habits-only-for-today nil
      org-agenda-show-future-repeats 'next
      org-agenda-start-day "-1d"
      org-agenda-start-on-weekday nil
      org-agenda-span 6)

(setq org-agenda-time-grid
      (quote
       ((daily today require-timed remove-match)
	nil
	"......" "----------------")))

(setq safe-local-variable-values '((eval org-shifttab 2)))


;; (add-to-list 'org-agenda-custom-commands
;;              '("l" "Show agenda with log and time report"
;; 	       ((agenda "" (org-agenda-clockreport-mode))
;; 		(org-agenda-log-mode))))
