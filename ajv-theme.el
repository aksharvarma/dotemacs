(provide 'ajv-theme)

(defun ajv/theme/using-dark-theme-p ()
  "Returns t if currently using prefered dark theme found in ajv/settings/prefered-dark-theme-name"
  (eql (car custom-enabled-themes) ajv/settings/prefered-dark-theme-name))

(defun ajv/theme/using-light-theme-p ()
  "Returns t if currently using prefered light theme found in ajv/settings/prefered-light-theme-name"
  (eql (car custom-enabled-themes) ajv/settings/prefered-light-theme-name))

(defun ajv/theme/toggle-between-themes ()
  "Toggle between my prefered theme and no theme (Emacs default theme).
My prefered theme is a dark theme which doesn't work well in bright light. The Emacs default theme is good enough for bright light."
  (interactive)
  (cond
   ((ajv/theme/using-light-theme-p)
    (disable-theme ajv/settings/prefered-light-theme-name)
    (enable-theme ajv/settings/prefered-dark-theme-name)
    (ajv/theme/set-region-face t))
   ((ajv/theme/using-dark-theme-p)
    (disable-theme ajv/settings/prefered-dark-theme-name)
    (enable-theme ajv/settings/prefered-light-theme-name)
    (ajv/theme/set-region-face nil))
   (t (enable-theme ajv/settings/prefered-dark-theme-name)))
  (ajv/god/update-cursor)
  (ajv/hl-line/set-face))

(defun ajv/theme/set-region-face (background-is-dark) (interactive)
       (if background-is-dark
	   (set-face-background 'region "dark slate gray")
	 (set-face-background 'region "light sky blue")))
