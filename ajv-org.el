(provide 'ajv-org)

(setq org-startup-truncated 'nil)
(setq org-catch-invisible-edits 'smart)
(setq org-special-ctrl-a/e t)
(setq org-link-search-must-match-exact-headline nil)
(setq org-goto-interface 'outline-path-completion)
(setq org-outline-path-complete-in-steps nil)
(setq org-hide-emphasis-markers t)
(setq org-highlight-latex-and-related '(native))
(setq org-list-allow-alphabetical t)
;; consider changing to 'note so that you add a note when finishing a task
;; See org-log-note-headings for more ideas
(setq org-log-done 'time)
(setq org-log-into-drawer t)
(setq org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "WAITING(w@)"
				    "ONGOING(o!/!)" "MEETING(m)" "INACTIVE(i!/!)"
				    "|" "DONE(d)" "CANCELLED(c@)")))
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


;; Capture templates for: TODO Tasks, Notes, emails, meetings, notes, research ideas
(setq org-capture-templates
      (quote (("t" "todo" entry (file+headline org-default-notes-file "Tasks")
               "* TODO %?\n%U\n%a\n")
	      ("c" "Communication" entry (file+headline org-default-notes-file "Communication")
               "* %? :COMMUNICATION:\n%U\n")
	      ("e" "Email related, ec for composing new ones, er for replies")
	      ("ec" "Email compose" entry (file+headline org-default-notes-file "Emails")
               "* Email %^{TO: WHO?} about %^{SUBJECT: ABOUT?}\nSCHEDULED: %t\n%U\n" :immediate-finish t)
	      ("er" "Email reply" entry (file+headline org-default-notes-file "Emails")
               "* Respond to %:from on %:subject\nSCHEDULED: %t\n%U\n%a\n" :immediate-finish t)
	      ("m" "Meeting" entry (file+headline org-default-notes-file "Meetings")
               "* MEETING with %? :MEETING:\n%U")
              ("n" "Note" entry (file+headline org-default-notes-file "Notes")
               "* %? :NOTE:\n%U\n%a\n%i")
              ("r" "Research Ideas" entry (file+headline org-default-notes-file "Research")
               "* %? :RESEARCH:\n%U\n%a\n")
              ;; ("j" "Journal" entry (file+datetree "~/git/org/diary.org")
              ;;  "* %?\n%U\n" :clock-in t :clock-resume t)
              ;; ("w" "org-protocol" entry (file org-default-notes-file)
              ;;  "* TODO Review %c\n%U\n" :immediate-finish t)
              ;; ("h" "Habit" entry (file org-default-notes-file)
              ;;  "* NEXT %?\n%U\n%a\nSCHEDULED: %(format-time-string \"%<<%Y-%m-%d %a .+1d/3d>>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n")
	      )))

;; Targets include this file and any file contributing to the agenda - up to 9 levels deep
(setq org-refile-targets (quote ((nil :maxlevel . 4)
                                 (org-agenda-files :maxlevel . 4))))

;; Use full outline paths for refile targets - we file directly with IDO
(setq org-refile-use-outline-path t)

;; Targets complete directly with IDO
(setq org-outline-path-complete-in-steps nil)

;; Allow refile to create parent tasks with confirmation
(setq org-refile-allow-creating-parent-nodes (quote confirm))

;; (defun ajv/org/clock-in-if-state-ongoing ()
;;   "clock in when the task is marked ongoing.

;; modified from: https://sachachua.com/blog/2007/12/clocking-time-with-emacs-org/"
;;   ;; the variables are prefixed with org- unlike what the link suggests.
;;   (when (and (string= org-state "ongoing")
;; 	     (not (string= org-last-state org-state)))
;;     (org-clock-in)))

;; (defun ajv/org/clock-out-switch-to-which-state (current-state)
;;   "this takes in the current state of a task and allows the user to choose between a few final-like states to make the new state. used as part of the org-clock-out-switch-to-state variable."
;;   (ido-completing-read (concat "current state is: " current-state
;; 			       " choose new state:")
;; 		       '("waiting" "cancelled" "done" "") nil nil nil nil "done"))

;; (setq org-clock-in-switch-to-state "ongoing")
;; (setq org-clock-out-switch-to-state 'ajv/org/clock-out-switch-to-which-state)

;; (defun ajv/org/remove-all-appts (&optional refresh filter &rest args)
;;   "remove all the appointment entries. this becomes a piece of advice before org-agenda-to-appt. idea from: https://sachachua.com/blog/2007/11/setting-up-appointment-reminders-in-org/"
;;   (interactive)
;;   (setq appt-time-msg-list nil))

;; (add-to-list 'org-agenda-custom-commands
;;              '("l" "Show agenda with log and time report"
;; 	       ((agenda "" (org-agenda-clockreport-mode))
;; 		(org-agenda-log-mode))))
