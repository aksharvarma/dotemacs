(provide 'ajv-magit)

;; Taken from: http://manuel-uberti.github.io/emacs/2018/02/17/magit-bury-buffer/
(defun mu-magit-kill-buffers ()
  "Restore window configuration and kill all Magit buffers."
  (interactive)
  (let ((buffers (magit-mode-get-buffers)))
    (magit-restore-window-configuration)
    (mapc #'kill-buffer buffers)))
