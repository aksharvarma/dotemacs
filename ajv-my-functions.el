(provide 'ajv-my-functions)

(defmacro ajv/make-mode-defuns (full-mode-name defun-name &optional val)
  "Create functions to enable/disable a given mode. This simply provides a wrapper around calls of the form `(foo-bar-mode 1)' or `(foo-bar-mode -1)'. Should make life easier for use in hooks."
  ;; Create a function named ajv/defun-name
  `(defun ,(intern defun-name) ()
     ;; Documentation for the function
     ,(concat "This function is simply a wrapper that calls `" (symbol-name full-mode-name) "` with argument " (number-to-string val))
     (interactive)
     ;; Call the function with argument val or 1.
     (,full-mode-name (or ,val 1))))

(defmacro ajv/make-enable-disable-defuns (full-mode-name defun-name &optional enable-val disable-val)
  "Create functions to enable and disable a given mode. This simply provides a wrapper around calls of the form `(foo-bar-mode 1)' and `(foo-bar-mode -1)'. Should make life easier for use in hooks."
  ;; Create an enable and a disable function
  `(progn
     (ajv/make-mode-defuns ,full-mode-name
			   ,(concat "ajv/" (symbol-name defun-name) "/enable")
			   ,(or enable-val 1))
     (ajv/make-mode-defuns ,full-mode-name
			   ,(concat "ajv/" (symbol-name defun-name) "/disable")
			   ,(or disable-val -1))))

;; Creates ajv/linum/enable and ajv/linum/disable
(ajv/make-enable-disable-defuns linum-mode linum 1 -1)


(defun ajv/setup-scratch-buffer ()
  "Setup the scratch buffer according to my tastes. If the buffer doesn't exist,
          create it and write the initial message into it."
  (interactive)
  (let ((scratch-buffer
	 (or (get-buffer ajv/settings/scratch-buffer-name)
	     (get-buffer (concat ajv/settings/scratch-buffer-name "[emacs-lisp]"))
	     (get-buffer (concat ajv/settings/scratch-buffer-name "[python]")))))
    (unless scratch-buffer
      (setq scratch-buffer (get-buffer-create ajv/settings/scratch-buffer-name))
      (with-current-buffer scratch-buffer
        (funcall initial-major-mode)
        (insert initial-scratch-message)
        (setq-local org-confirm-babel-evaluate nil)
        ;; (org-babel-execute-buffer)
	))))

(defun ajv/switch-buffer-scratch ()
  "Switch to the scratch buffer. If the buffer doesn't exist,
          create it and write the initial message into it."
  (interactive)
  (ajv/setup-scratch-buffer)
  (switch-to-buffer (or (get-buffer ajv/settings/scratch-buffer-name)
			(get-buffer (concat ajv/settings/scratch-buffer-name "[emacs-lisp]"))
			(get-buffer (concat ajv/settings/scratch-buffer-name "[python]")))))

(defun ajv/switch-buffer-scratch-other-window ()
  "Switch to the scratch buffer. If the buffer doesn't exist,
create it and write the initial message into it."
  (interactive)
  (other-window 1)
  (ajv/switch-buffer-scratch)
  ;; (other-window 1)
  )

(defun ajv/show-weekly-plan-table ()
  "Switch to the self.org file; close other windows, expand on the weekly plan table."
  (interactive)
  (switch-to-buffer "self.org")
  (goto-char (org-find-exact-headline-in-buffer "Weekly Plan"))
  ;; (org-narrow-to-subtree)
  (outline-show-subtree)
  (delete-other-windows)
  )


(defun ajv/open-home-in-dired ()
  (interactive)
  (dired "~/"))

(defun ajv/open-symlink-folder-in-dired ()
  (interactive)
  (dired ajv/settings/symlink-folder))

(setq ajv/never-kill-buffer-list
      '("*scratch*" "*scratch*[emacs-lisp]" "*scratch*[python]" "*Messages*"))

(defun ajv/kill-this-buffer ()
  "Reliably kill the current buffer. 'kill-this-buffer' is unreliable unless called from the menu-bar. See: http://pragmaticemacs.com/emacs/dont-kill-buffer-kill-this-buffer-instead/

Also ensure that buffers ajv/never-kill-buffer-list are not killed, only buried.
Taken from: https://www.reddit.com/r/emacs/comments/25xmji/bury_buffers_instead_of_killing_them/chlr8b2
"
  (interactive)
  (if (member (buffer-name (current-buffer)) ajv/never-kill-buffer-list)
      (call-interactively 'bury-buffer)
    (kill-buffer (current-buffer))))

(defun ajv/kill-other-buffer ()
  "Kill the other buffer in other window (whichever is the reached via (other-window 1))"
  (interactive)
  (other-window 1)
  (ajv/kill-this-buffer)
  (other-window 1)
  )

(defun ajv/match-paren (arg)
  "Go to the matching paren if on a paren; otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
	((looking-at "\\s\)") (forward-char 1) (backward-list 1))
	(t (self-insert-command (or arg 1)))))

(defun ajv/ask-before-closing ()
  "Ask whether or not to close, and then close if y was pressed.
Picked from: http://nileshk.com/2009/06/13/prompt-before-closing-emacs.html"
  (interactive)
  (if (yes-or-no-p (format "Are you sure you want to exit Emacs? "))
      (if (< emacs-major-version 22)
          (save-buffers-kill-terminal)
        (save-buffers-kill-emacs))
    (message "Canceled exit")))


(defun ajv/measure-loading-time ()
  "Simply measure and message the loading time."
  (interactive)
  (message "Emacs ready in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                    (time-subtract after-init-time before-init-time)))
           gcs-done))

(defun ajv/set-current-frame-as-primary-frame-reference ()
  "Sets windows according to my liking"
  (interactive)
  (setq ajv/settings/primary-frame-reference (selected-frame)))

(defun ajv/create-my-window-config ()
  "Sets windows according to my liking"
  (interactive)
  (unless (and ajv/settings/primary-frame-reference
	       (frame-live-p ajv/settings/primary-frame-reference))
    (ajv/set-current-frame-as-primary-frame-reference))
  (delete-other-windows)
  (split-window-horizontally)
  (org-agenda-list)
  ;; (other-window 1)
  (ibuffer t nil nil t nil)
  ;; (ajv/ibuffer/default-filter-folding)
  ;; (other-window 1)
  )

(defun ajv/create-my-window-config-in-primary-frame (&optional force)
  "Sets windows according to my liking but only if in the primary-frame"
  (interactive)
  (if force (ajv/create-my-window-config)
    (progn
      (unless ajv/settings/primary-frame-reference
	(setq ajv/settings/primary-frame-reference (selected-frame)))
      (select-frame ajv/settings/primary-frame-reference)
      (unless (and (buffer-file-name) (string-equal (file-name-extension (buffer-file-name)) "pdf"))
	(ajv/create-my-window-config))))
  )

(defun ajv/reopen-file-with-sudo ()
  "Advises ido-find-file to reopen current buffer with sudo permission"
  (interactive)
  (find-alternate-file (concat "/sudo::" buffer-file-name)))

(defun yes-or-no-p->-y-or-n-p (orig-fun &rest r)
  (cl-letf (((symbol-function 'yes-or-no-p) #'y-or-n-p))
    (apply orig-fun r)))

(defun ajv/ido-mypaths ()
  "Call ido-find-file after setting default-directory to be the symlink folder. Effectively mirrors the mypaths kind of behaviour."
  (interactive)
  (let ((default-directory (file-truename ajv/settings/symlink-folder)))
    (ido-find-file))
  )

(defun ajv/helm-mypath-helper (fname)
  (interactive)
  (let ((full-fname (concat (file-truename ajv/settings/symlink-folder) fname)))
    (if (file-directory-p full-fname)
	(helm-find-files-1 (concat full-fname "/"))
      (find-file-at-point full-fname))))

;; (defun ajv/helm-mypath-magit-helper (fname)
;;   (interactive)
;;   (magit-status fname))

(defun ajv/helm-mypaths ()
  "Call ido-find-file after setting default-directory to be the symlink folder. Effectively mirrors the mypaths kind of behaviour."
  (interactive)
  (let (default-directory (file-truename ajv/settings/symlink-folder))
    (helm
     :sources
     (helm-build-sync-source "Mypaths"
       :candidates (directory-files (file-truename ajv/settings/symlink-folder))
       :action (helm-make-actions "Enter" 'ajv/helm-mypath-helper))
     :buffer "*helm mypaths*")))

(defun ajv/mypaths ()
  (interactive)
  (ajv/helm-mypaths))

(defun ajv/mypaths-other-window ()
  "Call ido-find-file after setting default-directory to be the symlink folder. Effectively mirrors the mypaths kind of behaviour."
  (interactive)
  (other-window 1)
  (ajv/mypaths)
  )

(defun ajv/delete-trailing-whitespace ()
  "Delete trailing whitespace in files. Ideally would be hooked onto before-save-hook."
  (interactive)
  (when (not (or (derived-mode-p 'markdown-mode)
		 (derived-mode-p 'org-mode)))
    (delete-trailing-whitespace))
  )

(defun ajv/shell-command-on-buffer (command)
  "Execute a shell command on the buffer and replace contents with output of command."
  (interactive "sShell command on buffer: ")
  (shell-command-on-region (point-min) (point-max) command t)
  )


(defun xah/title-case-region-or-line (@begin @end)
  "Title case text between nearest brackets, or current line, or text selection.
Capitalize first letter of each word, except words like {to, of, the, a, in, or, and, …}. If a word already contains cap letters such as HTTP, URL, they are left as is.

When called in a elisp program, *begin *end are region boundaries.
URL `http://ergoemacs.org/emacs/elisp_title_case_text.html'
Version 2017-01-11"
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (let (
           $p1
           $p2
           ($skipChars "^\"<>(){}[]“”‘’‹›«»「」『』【】〖〗《》〈〉〔〕"))
       (progn
         (skip-chars-backward $skipChars (line-beginning-position))
         (setq $p1 (point))
         (skip-chars-forward $skipChars (line-end-position))
         (setq $p2 (point)))
       (list $p1 $p2))))
  (let* (
         ($strPairs [
                     [" A " " a "]
                     [" And " " and "]
                     [" At " " at "]
                     [" As " " as "]
                     [" By " " by "]
                     [" Be " " be "]
                     [" Into " " into "]
                     [" In " " in "]
                     [" Is " " is "]
                     [" It " " it "]
                     [" For " " for "]
                     [" Of " " of "]
                     [" Or " " or "]
                     [" On " " on "]
                     [" Via " " via "]
                     [" The " " the "]
                     [" That " " that "]
                     [" To " " to "]
                     [" Vs " " vs "]
                     [" With " " with "]
                     [" From " " from "]
                     ["'S " "'s "]
                     ["'T " "'t "]
                     ]))
    (save-excursion
      (save-restriction
        (narrow-to-region @begin @end)
        (upcase-initials-region (point-min) (point-max))
        (let ((case-fold-search nil))
          (mapc
           (lambda ($x)
             (goto-char (point-min))
             (while
                 (search-forward (aref $x 0) nil t)
               (replace-match (aref $x 1) "FIXEDCASE" "LITERAL")))
           $strPairs))))))

(defun xah/title-case-string (s)
  (with-temp-buffer
    (erase-buffer)
    (insert s)
    (xah/title-case-region-or-line (point-min)
				   (point-max))
    (buffer-substring-no-properties (point-min)
                                    (point-max))))


(defun xah/create-empty-buffer ()
  "Create a new empty buffer.
New buffer will be named “untitled” or “untitled<2>”, “untitled<3>”, etc.

It returns the buffer (for elisp programing).

URL `http://ergoemacs.org/emacs/emacs_new_empty_buffer.html'
Version 2017-11-01"
  (interactive)
  (let ((buf (generate-new-buffer (concat "empty-buffer-"
					  (number-to-string (random 123456))))))
    (switch-to-buffer buf)
    (write-file (concat "~/" (buffer-name)))
    (funcall (intern
	      (ido-completing-read
	       "Choose initial mode: "
	       (list "python-mode" "lisp-interaction-mode" "LaTex-mode" "text-mode" "org-mode")
	       nil nil nil nil "lisp-interaction-mode")))
    (setq buffer-offer-save t)
    buf))


(defun ajv/alsamixer ()
  "Runs the alsamixer program in an ansi-term"
  (interactive)
  (ansi-term "alsamixer")
  (rename-buffer "* terminal alsamixer *"))


(defun ajv/rename-symlink-buffer-with-truename ()
  (interactive)
  (when (buffer-file-name)
    (rename-buffer (file-name-nondirectory (file-truename (buffer-file-name))) t)))


(defun ajv/bashmount ()
  (interactive)
  (term "/bin/bashmount")
  (rename-buffer "* terminal bashmount *"))

(defun ajv/goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line number input

Taken from: http://whattheemacsd.com/key-bindings.el-01.html"
  (interactive)
  (unwind-protect
      (progn
        (ajv/linum/enable)
	(ajv/hl-line/enable)
        (goto-line (read-number "Goto line: ")))
    (ajv/linum/disable)
    (run-at-time "1 sec" nil 'ajv/hl-line/disable)))

(defun ajv/join-to-next-line ()
  "Join the next line into this one.

Taken: http://whattheemacsd.com/key-bindings.el-03.html"
  (interactive)
  (join-line -1))

(defun ajv/join-to-previous-line ()
  "Collapse this line into the previous one.

Taken: http://whattheemacsd.com/key-bindings.el-03.html"
  (interactive)
  (join-line))


(defun ajv/open-website-in-tramp ()
  "Open my website, location given by ajv/sensitive/website-tramp-foldername using TRAMP."
  (interactive)
  (find-file ajv/sensitive/website-tramp-foldername))

(defun ajv/remove-all-advice (sym)
  "Remove all advices from symbol SYM."
  (interactive "aFunction symbol: ")
  (advice-mapc (lambda (advice _props) (advice-remove sym advice)) sym))

(defun ajv/clear-kill-ring ()
  "Clear the whole kill-ring."
  (interactive)
  (progn (setq kill-ring nil) (garbage-collect)))

(defun ajv/remove-last-kill-ring-item ()
  "Clear the whole kill-ring."
  (interactive)
  (pop kill-ring))

(defun ajv/increase-frame-font (decreasep)
  (interactive "P")
  (let ((old-face-attribute (face-attribute 'default :height)))
    (if decreasep			;if prefix then decrease else increase
	(set-face-attribute 'default nil :height (- old-face-attribute 10))
      (set-face-attribute 'default nil :height (+ old-face-attribute 10)))))

(defun ajv/set-default-frame-font ()
  (interactive)
  (set-face-attribute 'default nil :height ajv/settings/default-frame-font-height-value))


(defun ajv/frames/is-primary-frame-p (&optional frame)
  (interactive)
  (let ((chosen-frame (if frame frame (selected-frame))))
    (if (eq chosen-frame ajv/settings/primary-frame-reference)
	(message "IS Primary frame")
      (message "NOT Primary frame"))))

(defun ajv/frames/is-notmuch-frame-p (&optional frame)
  (interactive)
  (let ((chosen-frame (if frame frame (selected-frame))))
    (if (eq chosen-frame ajv/settings/notmuch-frame-reference)
	(message "IS Notmuch frame")
      (message "NOT Notmuch frame"))))

(defun ajv/frames/set-as-primary-frame ()
  (interactive)
  (setq ajv/settings/primary-frame-reference (selected-frame)))

(defun ajv/frames/set-as-notmuch-frame ()
  (interactive)
  (setq ajv/settings/notmuch-frame-reference (selected-frame)))

;; TODO: Make a function to change all - to SPC or vice versa

(defun ajv/clear-image-cache ()
  (interactive)
  (clear-image-cache t))


;; (defun ajv/find-file ()
;;   (interactive)
;;   (when (not (featurep 'ido))
;;     (find-file)))

;; (defun ajv/find-file-other-window ()
;;   (interactive)
;;   (when (not (featurep 'ido))
;;     (find-file-other-window)))

;; (defun ajv/switch-buffer ()
;;   (interactive)
;;   (when (not (featurep 'ido))
;;     (switch-to-buffer)))

;; (defun ajv/find-file ()
;;   (interactive)
;;   (when (not (featurep 'ido))
;;     (switch-to-buffer-other-window)))
;; (fset 'ido-completing-read 'completing-read)
;; (fset 'ido-find-file 'find-file)
;; (fset 'ido-switch-buffer 'switch-to-buffer)
;; (fset 'ido-switch-buffer-other-window 'switch-to-buffer-other-window)


(defun ajv/auto-tangle-emacs-config ()
  (when (string-equal
	 (buffer-file-name)
	 (file-truename
	  (expand-file-name "~/0/.dotfiles/emacs-configuration.org")))
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))


(defun ajv/toggle-mouse-avoidance-mode ()
  (interactive)
  (if mouse-avoidance-mode
      (mouse-avoidance-mode 'none)
    (mouse-avoidance-mode 'banish)))


(defun ajv/space-to-hyphen ()
  (interactive)
  (when (use-region-p)
    (subst-char-in-region (region-beginning) (region-end) ?  ?-)))

(defun ajv/hyphen-to-space ()
  (interactive)
  (when (use-region-p)
    (subst-char-in-region (region-beginning) (region-end) ?- ? )))


;; James Dyer's indexing init file with occur
;; Allows going to each section
;; since the sections are delimited using
;; -> sectionname
(defun ajv/occur/index ()
  (interactive)
  (beginning-of-buffer)
  (occur ";;[[:space:]]->"))
