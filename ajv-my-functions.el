(provide 'ajv-my-functions)

(defun ajv/switch-buffer-scratch ()
  "Switch to the scratch buffer. If the buffer doesn't exist,
create it and write the initial message into it."
  (interactive)
  (let* ((scratch-buffer-name "*scratch*")
         (scratch-buffer (get-buffer scratch-buffer-name)))
    (unless scratch-buffer
      (setq scratch-buffer (get-buffer-create scratch-buffer-name))
      (with-current-buffer scratch-buffer
        (lisp-interaction-mode)
        (insert initial-scratch-message)))
    (switch-to-buffer scratch-buffer)))

(defun ajv/open-home-in-dired ()
  (interactive)
  (dired "~/"))

(defun ajv/open-symlink-folder-in-dired ()
  (interactive)
  (dired ajv/symlink-folder))

(defun ajv/kill-this-buffer ()
  "Reliably kill the current buffer. 'kill-this-buffer' is unreliable unless called from the menu-bar. See: http://pragmaticemacs.com/emacs/dont-kill-buffer-kill-this-buffer-instead/"
  (interactive)
  (kill-buffer (current-buffer)))

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


(defun ajv/hideshow-setup ()
  "Setup hideshow mode for current mode/buffer. Should be hooked to prog-mode-hook."
  (interactive)
  (local-set-key (kbd "C-c C-s") 'hs-show-block)
  (local-set-key (kbd "C-c C-h") 'hs-hide-block)
  (local-set-key (kbd "C-c C-M-h") 'hs-hide-all)
  (local-set-key (kbd "C-c C-M-s") 'hs-show-all)
  (hs-minor-mode 1))

(defun ajv/measure-loading-time ()
  "Simply measure and message the loading time."
  (interactive)
  (message "Emacs ready in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                    (time-subtract after-init-time before-init-time)))
           gcs-done))

(defun ajv/window-config ()
  "Sets windows according to my liking"
  (interactive)
  (delete-other-windows)
  (split-window-horizontally)
  (other-window 1)
  (ajv/switch-buffer-scratch)
  (other-window 1)
  (dired ajv/symlink-folder)
  )

(defun ajv/reopen-file-with-sudo ()
  "Advises ido-find-file to reopen current buffer with sudo permission"
  (interactive)
  (async-shell-command (concat "sudoedit " buffer-file-name)))

(defun yes-or-no-p->-y-or-n-p (orig-fun &rest r)
  (cl-letf (((symbol-function 'yes-or-no-p) #'y-or-n-p))
    (apply orig-fun r)))


(defun ajv/toggle-theme ()
  "Toggle between my prefered theme and no theme (Emacs default theme).
My prefered theme is a dark theme which doesn't work well in bright light. The Emacs default theme is good enough for bright light."
  (interactive)
  (if custom-enabled-themes
      (disable-theme ajv/prefered-theme-name)
    (load-theme ajv/prefered-theme-name))
  )

(defun ajv/mypaths ()
  "Call ido-find-file after setting default-directory to be the symlink folder. Effectively mirrors the mypaths kind of behaviour."
  (interactive)
  (let ((default-directory (file-truename ajv/symlink-folder)))
    (ido-find-file))
  )

(defun ajv/mypaths-other-window ()
  "Call ido-find-file after setting default-directory to be the symlink folder. Effectively mirrors the mypaths kind of behaviour."
  (interactive)
  (other-window 1)
  (ajv/mypaths)
  )

(defun ajv/delete-trailing-whitespace ()
  (interactive)
  (when (not (or (derived-mode-p 'markdown-mode)
		 (derived-mode-p 'org-mode)))
	     (delete-trailing-whitespace))
  )
