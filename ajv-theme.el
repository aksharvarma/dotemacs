(provide 'ajv-theme)

(defun ajv/theme/using-dark-theme-p ()
  "Returns t if currently using prefered dark theme found in ajv/settings/prefered-dark-theme-name"
  (interactive)
  (if (custom-theme-enabled-p ajv/settings/prefered-dark-theme-name)
      t nil))

(defun ajv/theme/using-light-theme-p ()
  "Returns t if currently using prefered light theme found in ajv/settings/prefered-light-theme-name"
  (interactive)
  (if (custom-theme-enabled-p ajv/settings/prefered-light-theme-name)
      t nil))

(defun ajv/theme/set-region-face (background-is-dark)
  (interactive)
  (if background-is-dark
      (set-face-background 'region "dark slate gray")
    (set-face-background 'region "light sky blue")))

(defun ajv/theme/toggle-themes-with-frame-closing (&optional new-theme)
  (interactive)
  (ajv/org-roam/ui-stop)
  ;; (dolist (frame (frame-list) nil)
  ;;   (delete-frame frame))
  (cond
   ((ajv/theme/using-dark-theme-p)
    (disable-theme ajv/settings/prefered-dark-theme-name)
    (setq ajv/settings/cursor-color-outside-god "black")
    (ajv/god/update-cursor)
    (ajv/theme/set-region-face nil)
    (enable-theme ajv/settings/prefered-light-theme-name))
   ((ajv/theme/using-light-theme-p)
    (disable-theme ajv/settings/prefered-light-theme-name)
    (setq ajv/settings/cursor-color-outside-god "lime green")
    (ajv/god/update-cursor)
    (ajv/theme/set-region-face t)
    (enable-theme ajv/settings/prefered-dark-theme-name))
   (t
    (setq ajv/settings/cursor-color-outside-god "lime green")
    (ajv/god/update-cursor)
    (ajv/theme/set-region-face t)
    (enable-theme ajv/settings/prefered-dark-theme-name)))
  (ajv/org-roam/ui-start))

(defun ajv/theme/switch-themes-with-frame-closing (&optional new-theme)
  (interactive)
  (ajv/org-roam/ui-stop)
  (dolist (frame (frame-list) nil)
    (delete-frame frame))
  (message "deleted all frames")
  (dolist (theme custom-enabled-themes nil)
    (disable-theme theme))
  (message "disabled all themes")
  (enable-theme (or new-theme ajv/settings/prefered-dark-theme-name))
  (message "enabled new theme")
  (ajv/frames/set-as-primary-frame (make-frame '((window-system . x))))
  (message "recreated frame")
  (ajv/org-roam/ui-start))

;; (defun ajv/theme/set-theme (name-list)
;;   (interactive)
;;   (setq ajv/settings/cursor-color-outside-god (nth 1 name-list))
;;   (ajv/god/update-cursor)
;;   (ajv/theme/set-region-face (nth 2 name-list))
;;   (ajv/theme/switch-themes-with-frame-closing (car name-list)))

;; (defun ajv/theme/helm ()
;;   (interactive)
;;   (helm
;;    :sources
;;    (helm-build-sync-source "Themes"
;;      :candidates `(("Dark" . (,ajv/settings/prefered-dark-theme-name "lime green" t))
;; 		   ("Light" . (,ajv/settings/prefered-light-theme-name "black" nil)))
;;      :action (helm-make-actions
;;               "Apply theme, region and god-cursor settings"
;;               'ajv/theme/set-theme))
;;    :buffer "*helm Themes*"))
