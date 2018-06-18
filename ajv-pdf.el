(provide 'ajv-pdf)
(defun pdf-view-move-modeline-to-top ()
  (interactive)
  (setq header-line-format mode-line-format
        mode-line-format nil)
  )
