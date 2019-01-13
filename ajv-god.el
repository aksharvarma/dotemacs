(provide 'ajv-god)

(defun ajv/god-update-cursor ()
  (if (bound-and-true-p god-local-mode)
      (set-cursor-color "dark orange")
    (set-cursor-color "lime green"))
  )
