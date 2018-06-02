(provide 'ajv-visual)
(load-theme 'deeper-blue)
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(setq-default fill-column most-positive-fixnum)
(setq-default visual-line-fringe-indicators '(nil right-curly-arrow))
(column-number-mode t)
(scroll-bar-mode 0)
;; (toggle-menu-bar-mode-from-frame 0)
(menu-bar-mode 0)
(tool-bar-mode 0)
(show-paren-mode t)
(set-fringe-style '(0 . nil))
(add-to-list 'default-frame-alist '(fullscreen . fullboth)) ;maximize all frames
(add-to-list 'default-frame-alist '(font . "dejavu sans mono 10"))

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
