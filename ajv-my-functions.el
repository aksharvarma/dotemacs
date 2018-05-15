
;;;>>>
;;;This ensures that all marked files open in one vlc.
;;;Emacs 24 has changed the dired-do-shell-command's 
;;;normal behavior and so this is necessary.
(defun vlc ()
  "Open marked files as single VLC playlist"
(interactive)
(dired-do-shell-command "vlc * &" nil (dired-get-marked-files)))
;;;}}}

;;;>>>
;;;This opens all files in the dired in vlc.
;;;Note: ALL files.
;;;NOTE: Unmarks everything that has been marked.
(defun play-all-in-vlc()
  "Open ALL files as single VLC playlist"
  (interactive)
  (dired-unmark-all-marks)
  (dired-toggle-marks)
  (dired-do-shell-command "vlc * &" nil (dired-get-marked-files))
  (dired-unmark-all-marks)
)

;;;}}}

;;;>>>
;;;This sets the yes-or-no-p thing so that it doesn't ask
;;;me whether to use a different buffer in case the defaul
;;;one is already in use.
(defadvice play-all-in-vlc (around stfu compile activate)
  (cl-flet ((yes-or-no-p (&rest args) t)
	 (y-or-n-p (&rest args) t))
    ad-do-it))
;;;}}}

;;;>>>
;;;This opens all files in the dired in vlc.
;;;Note: ALL files.
;;;NOTE: Unmarks everything that has been marked.
(defun play-mp3-in-vlc()
  "Open ALL files as single VLC playlist"
  (interactive)
  (dired-unmark-all-marks)
  (dired-mark-files-regexp "\.mp3$")
  (dired-do-shell-command "vlc * &" nil (dired-get-marked-files))
  (dired-unmark-all-marks)
)

;;;}}}

;;;>>>
;;;This sets the yes-or-no-p thing so that it doesn't ask
;;;me whether to use a different buffer in case the defaul
;;;one is already in use.
(defadvice play-mp3-in-vlc (around stfu compile activate)
  (cl-flet ((yes-or-no-p (&rest args) t)
	 (y-or-n-p (&rest args) t))
    ad-do-it))
;;;}}}


;;;>>>
;;;Changing iswitch mode to a more user-friendly keyset
;; (require 'edmacro)
;; (iswitchb-mode 1)

;; (defun iswitchb-local-keys ()
;;   (mapc (lambda (K) 
;; 	  (let* ((key (car K)) (fun (cdr K)))
;; 	    (define-key iswitchb-mode-map (edmacro-parse-keys key) fun)))
;; 	'(("<right>" . iswitchb-next-match)
;; 	  ("<left>"  . iswitchb-prev-match)
;; 	  ("<up>"    . ignore             )
;; 	  ("<down>"  . ignore             ))))

;; (add-hook 'iswitchb-define-mode-map-hook 'iswitchb-local-keys)
;;iswitch thing ends here
;;;}}}

;;;>>>
;;;Scratch buffer
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
;;function ends here
;;;}}}


;;;>>>
;;;Hiding hidden files in dired mode.
(require 'dired-x)
(setq dired-omit-files "^\\...+$")
(add-hook 'dired-mode-hook (lambda () (dired-omit-mode 1)))
;;;}}}

;;;>>>
;;;Closes the other buffer in the other window.
(defun close-other-buffer ()
  "Close the other buffer in other window"
  (interactive)
  (other-window 1)
  (kill-this-buffer)
  (other-window 1)
  )
;;;}}}

;;;>>>
;;;Define the function to launch file using other programs
(defun dired-launch-file ()
  "Launch system associated program on current file in dired buffer
modified from http://omniorthogonal.blogspot.in/2008/05/useful-emacs-dired-launch-hack.html"
  (interactive)
  (case system-type 
    (gnu/linux (let ((process-connection-type nil)) 
                 (start-process "*launch*" nil "xdg-open" (dired-get-filename))))
    (windows-nt (w32-shell-execute "open"  (dired-get-filename) nil nil))))


;; bind the function to the l key only in dired mode
(add-hook 'dired-mode-hook              ; some key bindings in dired
	  (lambda ()
	    (define-key dired-mode-map [?l] 'dired-launch-file) ; Launch file at cursor in associated program (l for launch)
	    ))
;;;}}}


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;>>>
;;;The function that highlights matching parenthesis.
(defun match-paren (arg)
  "Go to the matching paren if on a paren; otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
	((looking-at "\\s\)") (forward-char 1) (backward-list 1))
	(t (self-insert-command (or arg 1)))))
;;;}}}

;;;>>>
;;;This sets the key f11 to fullscreen(fully maxxed) emacs
(defun toggle-fullscreen ()
  "Toggle full screen on X11"
  (interactive)
  (when (eq window-system 'x)
    (set-frame-parameter
     nil 'fullscreen
     (when (not (frame-parameter nil 'fullscreen)) 'fullboth))))

;;;}}}



;;;>>>
;;;This confirms before closing the emacs window.
;;;http://nileshk.com/2009/06/13/prompt-before-closing-emacs.html
(defun ask-before-closing ()
  "Ask whether or not to close, and then close if y was pressed"
  (interactive)
  (if (yes-or-no-p (format "Are you sure you want to exit Emacs? "))
      (if (< emacs-major-version 22)
          (save-buffers-kill-terminal)
        (save-buffers-kill-emacs))
    (message "Canceled exit")))

(when window-system
  (global-set-key (kbd "C-x C-c") 'ask-before-closing))
;;;}}}



;;;>>>
;;; This enables the hs-minor-mode in C buffers, and puts interesting
;;; keybindings for them.
;;; http://emacs-fu.blogspot.in/2008/12/showing-and-hiding-blocks-of-code.html
(add-hook 'c-mode-common-hook
  (lambda()
    (local-set-key (kbd "C-c <right>") 'hs-show-block)
    (local-set-key (kbd "C-c <left>")  'hs-hide-block)
    (local-set-key (kbd "C-c <up>")    'hs-hide-all)
    (local-set-key (kbd "C-c <down>")  'hs-show-all)
    (hs-minor-mode t)))

;; Other possible options include at least these.
;; (add-hook 'c-mode-common-hook   'hs-minor-mode)
;; (add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)
;; (add-hook 'java-mode-hook       'hs-minor-mode)
;; (add-hook 'lisp-mode-hook       'hs-minor-mode)
;; (add-hook 'perl-mode-hook       'hs-minor-mode)
;; (add-hook 'sh-mode-hook         'hs-minor-mode)

;;;}}}

