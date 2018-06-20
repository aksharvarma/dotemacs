(provide 'ajv-pdf)
(defun ajv/pdf-view-move-modeline-to-top ()
  (interactive)
  (setq header-line-format mode-line-format
        mode-line-format nil)
  )
