(provide 'ajv-theme)

(defun ajv/theme/using-dark-theme-p ()
  "Returns t if currently using prefered dark theme found in ajv/prefered-dark-theme-name"
  (eql (car custom-enabled-themes) ajv/prefered-dark-theme-name))

(defun ajv/theme/using-light-theme-p ()
  "Returns t if currently using prefered light theme found in ajv/prefered-light-theme-name"
  (eql (car custom-enabled-themes) ajv/prefered-light-theme-name))

(defun ajv/theme/toggle-between-themes ()
  "Toggle between my prefered theme and no theme (Emacs default theme).
My prefered theme is a dark theme which doesn't work well in bright light. The Emacs default theme is good enough for bright light."
  (interactive)
  (cond
   ((ajv/theme/using-light-theme-p)
    (disable-theme ajv/prefered-light-theme-name)
    (enable-theme ajv/prefered-dark-theme-name))
   ((ajv/theme/using-dark-theme-p)
    (disable-theme ajv/prefered-dark-theme-name)
    (enable-theme ajv/prefered-light-theme-name))
   (t (enable-theme ajv/prefered-dark-theme-name))))
