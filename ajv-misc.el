(provide 'ajv-misc)
;;;;This contains all the miscellenous things. Mainly
;;; the single liners and such.

(load-theme 'deeper-blue)

;;Stop the startup screen from showing
(setq inhibit-startup-message t)
  ;; (get-buffer (dired "~/")))

;; (global-set-key (kbd "C-c w c") 'ajv-window-config)
;; (if (daemonp)
;;     (add-hook 'after-make-frame-functions
;;               (lambda (frame)
;;                 (setq initial-buffer-choice "~/")))
;;   (setq initial-buffer-choice 'ajv-window-config))

;; (setq initial-buffer-choice "~/")

;; (add-hook 'after-make-frame-functions
;;           (lambda (frame)
;;             (with-selected-frame frame
;;               (ajv-window-config))))

;;;Since in this ubuntu python is by default python2.7
;;;This line sets the default interpreter to use python3
;; TODO
;; (setq python-shell-interpreter "python")

;;;Word wrapping
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(setq-default fill-column most-positive-fixnum)
(setq-default visual-line-fringe-indicators '(nil right-curly-arrow))
;; For ORG mode
(setq org-startup-truncated 'nil)

;;;Line numbering on the side in all modes.
(global-linum-mode 0)

;;;Column numbers in all modes.
(setq column-number-mode t)

;; Self-explanatory. 0 removes the menu bar, 1 adds it.
(toggle-menu-bar-mode-from-frame 0)
(menu-bar-mode 0)
(tool-bar-mode 0)
(show-paren-mode t)

;; Misc picked up from customizations:
(setq LaTeX-command "latex -shell-escape")
(set-fringe-style '(0 . nil))
(setq org-todo-keywords (quote ((sequence "TODO(t)" "|" "IN-PROGRESS(i)" "|" "DONE(d)"))))

;;;Adding new line if marker is at end of buffer.
(setq next-line-add-newlines t)

;; ;;;default to other dired window for copy and such things
;; (setq dired-dwim-target t)

;;;Show matching parenthesis
(show-paren-mode 1)

;;;Typing Replaces Selection
(pending-delete-mode 1)			;XEmacs
;; OR/AND this line (they should be aliases but just in case)
(delete-selection-mode t)		;Emacs

;;;Enables copying and cutting into other programs
(setq select-enable-clipboard t)

;;;Enables copying and cutting from other programs
(setq interprogram-paste-function 'x-selection-value)

;; ;;;Making directories come first in dired mode
;; (setq dired-listing-switches "-a -l -L -h --group-directories-first")

;;;Use the next lines if using windows
;;;(setq ls-list-dir-first t)

;;;This is what lets 'a' work in the dired mode (was inserted automatically 
;;by emacs)
;; (put 'dired-find-alternate-file 'disabled nil)


;;;This allows auto-complete in all modes.
(global-auto-complete-mode t)


;; Enable upcase and downcase region
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)


;;;So that emacs opens previous saved session
;;;(desktop-save-mode 1)

;; To disable the menu bar, place the following line in your .emacs file:
;; (menu-bar-mode -1)

;; To disable the scrollbar, use the following line:
(toggle-scroll-bar -1)

;; To disable the toolbar, use the following line:
(tool-bar-mode -1)

;; Show time, date, day and battery status in the modeline.
;; Do it in this font for better visibility
(defface egoge-display-time
   '((((type x w32 mac))
      ;; #060525 is the background colour of my default face.
      (:foreground "#99FF00" :background "#121212" :inherit bold))
     (((type tty))
      (:foreground "#99FF00" :background "#121212")))
   "Face used to display the time in the mode line.")

;; This causes the current time in the mode line to be displayed in
 ;; `egoge-display-time-face' to make it stand out visually.
 (setq display-time-string-forms
       '((propertize (concat " " 24-hours ":" minutes " ")
                     'face 'egoge-display-time)))

(setq display-time-24hr-format t)       ;Use 24 hr format
(setq display-time-day-and-date t)      ;Also show day and date
(display-time-mode 1)                   ;Actually show the time
(display-battery-mode 1)                ;Enable battery display


;; To make AUCTeX read/update on changes to .bib files.
(setq TeX-parse-self nil) ; Enable parse on load. [DISABLED]
(setq TeX-auto-save nil) ; Enable parse on save. [DISABLED]

;; Don't use tabs while indenting.
(setq-default indent-tabs-mode nil)

;; Should maximize any frame that is created. 
(add-to-list 'default-frame-alist '(fullscreen . fullboth))

;; Set default font to the following
;; Adds this customization per frame.
(add-to-list 'default-frame-alist '(font . "dejavu sans mono 10"))
;; Was the following line but changed so as to make it happen at the right time.
;; (set-frame-font "dejavu sans mono 10" nil t)

;; Ensure that time-stamps are set before saving a file.
(add-hook 'before-save-hook 'time-stamp)
;; Set default pattern to nil so that local variables can be used.
(setq time-stamp-pattern nil)

;; This ensures that the details are not visible all the time.
;; (add-hook 'dired-mode-hook
;;       (lambda ()
;;         (dired-hide-details-mode)))


(winner-mode 1)

;; Save customization from the menu in the following file and load that file.
(setq custom-file (concat ajv/my-init-directory "ajv-customizations.el"))
(load custom-file)
