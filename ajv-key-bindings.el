;;;>>>
;;;These are all the keybindings.
;;;Killing current buffer.
(global-set-key (kbd "C-x x") 'kill-this-buffer)

;;;Closing other buffer
(global-set-key (kbd "C-x c") 'close-other-buffer)

;;;Burying buffer
(global-set-key (kbd "C-x z") 'bury-buffer)

;;;Compiling with Ctrl+<f9>
(global-set-key (kbd "C-<f9>") 'compile)

;;;Changing buffer with Ctrl+<tab>
(global-set-key (kbd "C-<tab>") 'other-window)

;;;Changing to scratch buffer using Ctrl+*
(global-set-key (kbd "C-*") 'switch-buffer-scratch)

;;;Opens home folder in dired on using control-tilde

(global-set-key (kbd "C-~")
		(lambda ()
		  (interactive)
		  (dired "~/")))



;; A few redefinitions that allow easier erasing of text.

(defun my-delete-char ()
  "my delete char"
  (interactive)
  (delete-char 1))

;; I am redefining down-list with this change
;; (global-set-key (kbd "C-M-d") 'my-delete-char)

(defun my-delete-backward-char ()
  "my delete char backwards"
  (interactive)
  (delete-char -1))

;; (global-set-key (kbd "C-d") 'my-delete-backward-char)


;; These have been taken from someone's code.
(defun my-delete-word (arg)
  "Delete characters forward until encountering the end of a word.
With argument, do this that many times.
This command does not push text to `kill-ring'."
  (interactive "p")
  (delete-region
   (point)
   (progn
     (forward-word arg)
     (point))))

(defun my-backward-delete-word (arg)
  "Delete characters backward until encountering the beginning of a word.
With argument, do this that many times.
This command does not push text to `kill-ring'."
  (interactive "p")
  (my-delete-word (- arg)))

;; Merely setting the key binding for the above functions.
;; (global-set-key (kbd "M-S-d") 'my-delete-word)

;; (global-set-key (kbd "M-d") 'my-backward-delete-word)


;;;This will make the <%> key show the matching parenthesis,
;;;like in vi.
;;;In addition, if the cursor isn't over a parenthesis, 
;;;it simply inserts a % like normal.
;; (global-set-key "%" 'match-paren)

;;;Fullscreening using <f11>
(global-set-key [f11] 'toggle-fullscreen)

;;;}}}

;; (add-hook 'LaTeX-mode-hook '(lambda() () (define-key LaTeX-mode-map (kbd "C-c C-h") )


(defun delete-backup-files ()
  "Delete all backup files in the current dired folder"
  (interactive)
  (dired-omit-mode 0)
  (dired-unmark-all-marks)
  (dired-flag-backup-files)
  (dired-do-flagged-delete))

(global-set-key (kbd "C-c C-d C-b") 'delete-backup-files)

(global-set-key (kbd "<f3>") 'ido-switch-buffer)
(global-set-key (kbd "<f12>") 'save-buffer)
(global-set-key (kbd "<f8>") 'other-window)
(global-set-key (kbd "<f7>") 'kill-this-buffer)
(global-set-key (kbd "<f9>") 'close-other-buffer)


(global-set-key (kbd "C-C C-W C-c") 'ajv-window-config)


;; Don't remember. Check one day.
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(global-set-key (kbd "C-x g") 'magit-status)
