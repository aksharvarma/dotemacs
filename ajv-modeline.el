(provide 'ajv-modeline)

;; This causes the current time in the mode line to be displayed in
;; `ajv/display-time-face' to make it stand out visually.
(setq display-time-string-forms
      '((propertize (concat " " 24-hours ":" minutes " ")
		    'face 'ajv/display-time-face)))

(setq display-time-24hr-format t       ;Use 24 hr format
      display-time-day-and-date t)      ;Also show day and date
(display-time-mode 1)                   ;Actually show the time
(display-battery-mode 1)                ;Enable battery display
