(provide 'ajv-my-functions)

;; ;; TODO: All vlc related need to change to mpv
;; ;;;>>>
;; ;;;This ensures that all marked files open in one vlc.
;; ;;;Emacs 24 has changed the dired-do-shell-command's 
;; ;;;normal behavior and so this is necessary.
;; (defun vlc ()
;;   "Open marked files as single VLC playlist"
;; (interactive)
;; (dired-do-shell-command "vlc * &" nil (dired-get-marked-files)))
;; ;;;}}}

;; ;;;>>>
;; ;;;This opens all files in the dired in vlc.
;; ;;;Note: ALL files.
;; ;;;NOTE: Unmarks everything that has been marked.
;; (defun play-all-in-vlc()
;;   "Open ALL files as single VLC playlist"
;;   (interactive)
;;   (dired-unmark-all-marks)
;;   (dired-toggle-marks)
;;   (dired-do-shell-command "vlc * &" nil (dired-get-marked-files))
;;   (dired-unmark-all-marks)
;; )

;; ;;;}}}

;; ;;;>>>
;; ;;;This sets the yes-or-no-p thing so that it doesn't ask
;; ;;;me whether to use a different buffer in case the defaul
;; ;;;one is already in use.
;; (defadvice play-all-in-vlc (around stfu compile activate)
;;   (cl-flet ((yes-or-no-p (&rest args) t)
;; 	 (y-or-n-p (&rest args) t))
;;     ad-do-it))
;; ;;;}}}

;; ;;;>>>
;; ;;;This opens all files in the dired in vlc.
;; ;;;Note: ALL files.
;; ;;;NOTE: Unmarks everything that has been marked.
;; (defun play-mp3-in-vlc()
;;   "Open ALL files as single VLC playlist"
;;   (interactive)
;;   (dired-unmark-all-marks)
;;   (dired-mark-files-regexp "\.mp3$")
;;   (dired-do-shell-command "vlc * &" nil (dired-get-marked-files))
;;   (dired-unmark-all-marks)
;; )

;; ;;;}}}

;; ;;;>>>
;; ;;;This sets the yes-or-no-p thing so that it doesn't ask
;; ;;;me whether to use a different buffer in case the defaul
;; ;;;one is already in use.
;; (defadvice play-mp3-in-vlc (around stfu compile activate)
;;   (cl-flet ((yes-or-no-p (&rest args) t)
;; 	 (y-or-n-p (&rest args) t))
;;     ad-do-it))
;; ;;;}}}

(defun switch-buffer-scratch ()
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

(defun open-home-in-dired ()
  (interactive)
  (dired "~/"))

;;;>>>
;;;Hiding hidden files in dired mode.
;; (require 'dired-x)
;; (setq dired-omit-files "^\\...+$")
;; (add-hook 'dired-mode-hook (lambda () (dired-omit-mode 1)))
;;;}}}

(defun close-other-buffer ()
  "Close the other buffer in other window (whichever is the reached via (other-window 1))"
  (interactive)
  (other-window 1)
  (kill-this-buffer)
  (other-window 1)
  )

(defun dired-launch-file ()
  "Launch system associated program on current file in dired buffer
modified from http://omniorthogonal.blogspot.in/2008/05/useful-emacs-dired-launch-hack.html"
  (interactive)
  (case system-type
    (gnu/linux (let ((process-connection-type nil)) 
                 (start-process "*launch*" nil "xdg-open" (dired-get-filename))))
    (windows-nt (w32-shell-execute "open"  (dired-get-filename) nil nil))))

(defun match-paren (arg)
  "Go to the matching paren if on a paren; otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
	((looking-at "\\s\)") (forward-char 1) (backward-list 1))
	(t (self-insert-command (or arg 1)))))

(defun toggle-fullscreen ()
  "Toggle full screen on X11. Consider removing, have fullscreen on some frame-alist"
  (interactive)
  (when (eq window-system 'x)
    (set-frame-parameter
     nil 'fullscreen
     (when (not (frame-parameter nil 'fullscreen)) 'fullboth))))

(defun ask-before-closing ()
  "Ask whether or not to close, and then close if y was pressed.
Picked from: http://nileshk.com/2009/06/13/prompt-before-closing-emacs.html"
  (interactive)
  (if (yes-or-no-p (format "Are you sure you want to exit Emacs? "))
      (if (< emacs-major-version 22)
          (save-buffers-kill-terminal)
        (save-buffers-kill-emacs))
    (message "Canceled exit")))

(defun delete-backup-files ()
  "Delete all backup files in the current dired folder"
  (interactive)
  (dired-omit-mode 0)
  (dired-unmark-all-marks)
  (dired-flag-backup-files)
  (dired-do-flagged-delete))

(defun hideshow-setup ()
  "Setup hideshow mode for current mode/buffer. Should be hooked to prog-mode-hook."
  (interactive)
  (local-set-key (kbd "C-c C-s") 'hs-show-block)
  (local-set-key (kbd "C-c C-h") 'hs-hide-block)
  (local-set-key (kbd "C-c C-M-h") 'hs-hide-all)
  (local-set-key (kbd "C-c C-M-s") 'hs-show-all)
  (hs-minor-mode 1))

(defun measure-loading-time ()
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
  (switch-buffer-scratch)
  (other-window 1)
  (dired "~/")
  )

(defun reopen-file-with-sudo ()
  "Advises ido-find-file to reopen current buffer with sudo permission"
  (interactive)
  (async-shell-command (concat "sudoedit " buffer-file-name)))

(defun yes-or-no-p->-y-or-n-p (orig-fun &rest r)
  (cl-letf (((symbol-function 'yes-or-no-p) #'y-or-n-p))
    (apply orig-fun r)))


(defun pdf-view-move-modeline-to-top ()
  (interactive)
  (setq header-line-format mode-line-format)
  (setq mode-line-format nil)
  )

;; ;; The following three functions are currently unused.
;; (defun ajv-set-modeline-color-scheme ()
;;   "Make active modeline more visible and inactive one less visible, picked up from https://stackoverflow.com/questions/9446673/asking-emacs-to-highlight-more-clearly-which-window-pane-has-the-focus-cursor
;; Call after changing theme."
;;   (interactive nil)
;;   (progn
;;     (set-face-attribute  'mode-line
;;                          nil 
;;                          :foreground "black"
;;                          :background "gray80"
;;                          :box '(:line-width 1 :style released-button))
;;     (set-face-attribute  'mode-line-inactive
;;                          nil 
;;                          :foreground "gray50"
;;                          :background "grey20"
;;                          :box '(:line-width 1 :style released-button))))


;; (defun ajv-set-theme (chosen-theme)
;;   "Set the theme, modeline color scheme and cursor color to what I want."
;;   (interactive S)
;;   (progn
;;     (enable-theme chosen-theme)
;;     (ajv-set-modeline-color-scheme)
;;     ;; (set-cursor-color "yellow")
;;     ))


;; (defun ajv-cycle-theme ()
;;   "Cycle Emacs theme among a preset list.
;; Code modified from: `http://ergoemacs.org/emacs/elisp_toggle_command.html'
;; Version 2018-04-25"
;;   (interactive nil)
;;   ;; uses a property “state”. Value is a integer.
;;   (let* ((values [misterioso wombat manoj-dark])
;;          ;; (values ["wombat" "manoj-dark" "misterioso"])
;;          (index-before (if (get 'ajv-cycle-theme 'current-theme)
;;                            (get 'ajv-cycle-theme 'current-theme) 0))
;;          (index-after (% (+ index-before 1)
;;                           (length values)))
;;          (next-value (aref values index-after)))

;;     (progn
;;       (put 'ajv-cycle-theme 'current-theme index-after)
;;       (ajv-set-theme next-value)
;;       (message "Theme changed to %s" next-value))))
