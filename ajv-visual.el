(provide 'ajv-visual)
(load-theme ajv/prefered-light-theme-name t t)
(load-theme ajv/prefered-dark-theme-name)

(add-hook 'text-mode-hook 'turn-on-auto-fill)
(setq-default fill-column most-positive-fixnum
              visual-line-fringe-indicators '(nil right-curly-arrow))
(column-number-mode t)
(scroll-bar-mode 0)
(menu-bar-mode 0)
(tool-bar-mode 0)
(show-paren-mode t)
(set-fringe-style '(0 . nil))
(add-to-list 'default-frame-alist '(fullscreen . fullboth)) ;maximize all frames
(add-to-list 'default-frame-alist `(font . ,ajv/prefered-font-name))

(defun ajv/using-dark-theme-p ()
  "Returns t if currently using prefered dark theme found in ajv/prefered-dark-theme-name"
  (eql (car custom-enabled-themes) ajv/prefered-dark-theme-name))

(defun ajv/using-light-theme-p ()
  "Returns t if currently using prefered light theme found in ajv/prefered-light-theme-name"
  (eql (car custom-enabled-themes) ajv/prefered-light-theme-name))

(defun ajv/toggle-between-themes ()
  "Toggle between my prefered theme and no theme (Emacs default theme).
My prefered theme is a dark theme which doesn't work well in bright light. The Emacs default theme is good enough for bright light."
  (interactive)
  (cond
   ((ajv/using-light-theme-p)
    (disable-theme ajv/prefered-light-theme-name)
    (enable-theme ajv/prefered-dark-theme-name))
   ((ajv/using-dark-theme-p)
    (disable-theme ajv/prefered-dark-theme-name)
    (enable-theme ajv/prefered-light-theme-name))
   (t (enable-theme ajv/prefered-dark-theme-name))))
