(provide 'ajv-visual)
(load-theme 'deeper-blue)
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
(add-to-list 'default-frame-alist '(font . "dejavu sans mono 10"))
