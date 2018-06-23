(provide 'ajv-pdf)

(defvar ajv/pdf-mode-modeline-format)

(defun ajv/pdf-view-move-modeline-to-top ()
  "Removes mode-line when in pdf-view-mode. Use toggle function to show/hide."
  (interactive)
  (setq ajv/pdf-mode-modeline-format mode-line-format
        mode-line-format nil)
  )

;; Because pdf-tools is known to have issue with this.
(defun ajv/pdf-view-disable-linum-mode () (linum-mode 0))

(defun ajv/pdf-view-toggle-modeline ()
  "Toggles displaying the header-line when in pdf-view-mode."
  (interactive)
  (if header-line-format
      (setq header-line-format nil)
    (setq header-line-format ajv/pdf-mode-modeline-format))
  (force-mode-line-update)		;Needed to actually see the change
  )
