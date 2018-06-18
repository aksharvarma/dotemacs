(provide 'ajv-modeline)
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

(setq display-time-24hr-format t       ;Use 24 hr format
      display-time-day-and-date t)      ;Also show day and date
(display-time-mode 1)                   ;Actually show the time
(display-battery-mode 1)                ;Enable battery display
