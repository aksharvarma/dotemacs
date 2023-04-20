(provide 'ajv-org)

;; Some of the following editing ideas come from: https://github.com/minad/org-modern
(setq org-auto-align-tags nil)
(setq org-tags-column 5)
(setq org-catch-invisible-edits 'smart)
(setq org-special-ctrl-a/e t)

(setq org-startup-truncated 'nil)
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


;; https://lepisma.xyz/2017/10/28/ricing-org-mode/
(setq org-startup-indented t)
;; (setq org-bullets-bullet-list '("·"))
(setq org-ellipsis "↴")
(setq org-ellipsis "⛛")
(setq org-pretty-entities nil)
(setq org-hide-emphasis-markers t)
(setq org-fontify-whole-heading-line nil)
(setq org-fontify-done-headline nil)
(setq org-fontify-quote-and-verse-blocks t)


;; (message "Messaging: before setq for org-agenda-files")
;; (message (car ajv/settings/my-org-agenda-files))
;; Called from ajv-init.el instead.
;; (setq org-agenda-files ajv/settings/my-org-agenda-files)
;; (setq org-directory ajv/settings/my-org-agenda-files)
;; (message "Messaging: after setq")

;; (setq org-tags-column -90)

(setq org-agenda-skip-scheduled-if-done t)
(setq org-habit-show-habits-only-for-today t)
(setq org-habit-preceding-days 15)
(setq org-habit-graph-column 45)
(setq org-agenda-show-future-repeats 'next)
(setq org-agenda-start-on-weekday nil)
;; (setq org-agenda-start-day "-1d")
(setq org-agenda-start-day "+0d")
(setq org-agenda-span 'day)
(setq org-agenda-start-with-clockreport-mode nil)
(setq org-agenda-start-with-log-mode t)
;; org-agenda-span 6

(setq org-priority-lowest ?C)
(setq org-priority-default ?B)
(setq org-priority-highest ?A)
(setq org-priority-start-cycle-with-default t)

;; (setq org-priority-lowest 60)
;; (setq org-priority-default 15)
;; (setq org-priority-highest 1)
;; (setq org-priority-start-cycle-with-default t)


(setq org-agenda-window-setup 'current-window)
(setq org-agenda-log-mode-items '(closed close))
(setq org-agenda-time-grid
      (quote
       ((daily today require-timed remove-match)
        nil
        " ┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")))
(setq org-agenda-current-time-string
      "⭠ now ─────────────────────────────────────────────────")
;; (setq org-agenda-time-grid
;;       (quote
;;        ((daily today require-timed remove-match)
;; 	nil
;; 	"..." "-----------")))

;; Capture templates for: TODO Tasks, Notes, emails, meetings, notes, research ideas
(setq org-capture-templates
      `(("t" "todo" entry (file+headline org-default-notes-file "Tasks")
         "* TODO %?\nSCHEDULED: %t\n%U\n%a\n")
	("c" "Communication related, ec for calls, ee for emails, em for messages.")
	("cc" "Communication - Call" entry (file+olp "personal.org" "Communication" "Call")
         "* TODO %? :COMMUNICATION:\nSCHEDULED: %t\n%U\n")
	("ce" "Communication - Email" entry (file+olp "personal.org" "Communication" "Email")
         "* TODO %? :COMMUNICATION:\nSCHEDULED: %t\n%U\n")
	("cm" "Communication - Message" entry (file+olp "personal.org" "Communication" "Message")
         "* TODO %? :COMMUNICATION:\nSCHEDULED: %t\n%U\n")
	("e" "Email related, ec for composing new ones, er for replies.")
	("ec" "Email compose" entry (file+headline org-default-notes-file "Emails")
	 "* TODO Email %^{TO: WHO?} about %^{SUBJECT: ABOUT?}\nSCHEDULED: %t\n%U\n" :immediate-finish t)
	("er" "Email reply" entry (file+headline org-default-notes-file "Emails")
	 "* TODO Respond to %:from on %:subject\nSCHEDULED: %t\n%U\n%a\n" :immediate-finish t)
	("m" "Meeting" entry (file+headline org-default-notes-file "Meetings")
	 "* MEETING with %? :MEETING:\nSCHEDULED: %t\n%U\n%^{Effort|5m}p")
	("n" "Note" entry (file+headline org-default-notes-file "Notes")
	 "* TODO %? :NOTE:\nSCHEDULED: %t\n%U\n%a\n%i")
	("r" "Research Ideas" entry (file+olp+datetree "research.org")
	 "* TODO %?  :RESEARCH:\nSCHEDULED: %t\n%U\n%a\n")
	("s" "Surfing journal" entry (file+olp+datetree ajv/settings/surfing-journal)
	 "* %?\n%U\n")
	;; ("j" "Journal" entry (file+datetree "~/git/org/diary.org")
	;;  "* %?\n%U\n" :clock-in t :clock-resume t)
	;; ("w" "org-protocol" entry (file org-default-notes-file)
	;;  "* TODO Review %c\n%U\n" :immediate-finish t)
	;; ("h" "Habit" entry (file org-default-notes-file)
	;;  "* NEXT %?\n%U\n%a\nSCHEDULED: %(format-time-string \"%<<%Y-%m-%d %a .+1d/3d>>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n")
	))

;; Targets include this file and any file contributing to the agenda - up to 9 levels deep
(setq org-refile-targets (quote ((nil :maxlevel . 2)
				 (org-agenda-files :maxlevel . 2))))

;; Use full outline paths for refile targets - we file directly with IDO
(setq org-refile-use-outline-path 'file)

;; Targets complete directly with IDO
(setq org-outline-path-complete-in-steps nil)

;; Allow refile to create parent tasks with confirmation
(setq org-refile-allow-creating-parent-nodes (quote confirm))

(defun ajv/org/search ()
  (interactive)
  (org-refile '(4)))

;; https://emacs.stackexchange.com/a/69717
(defun ajv/org/open-link-in-current-window ()
  "Opens file in current window."
  (interactive)
  (let ((org-link-frame-setup (cons (cons 'file 'find-file) org-link-frame-setup)))
    (org-open-at-point)))

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
