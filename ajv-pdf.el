(provide 'ajv-pdf)

(defvar ajv/pdf-tools/mode-modeline-format)

(defun ajv/pdf-tools/save-disable-modeline-format ()
  "Removes mode-line when in pdf-view-mode. Use toggle function to show/hide."
  (interactive)
  (setq ajv/pdf-tools/mode-modeline-format mode-line-format
        mode-line-format nil)
  )

;; Because pdf-tools is known to have issue with this.
(defun ajv/pdf-tools/disable-linum-mode () (linum-mode 0))

(defun ajv/pdf-tools/toggle-headerline ()
  "Toggles displaying the header-line when in pdf-view-mode."
  (interactive)
  (if header-line-format
      (setq header-line-format nil)
    (setq header-line-format ajv/pdf-tools/mode-modeline-format))
  (force-mode-line-update)		;Needed to actually see the change
  )

(defun ajv/pdf-tools/toggle-modeline ()
  "Toggles displaying the mode-line when in pdf-view-mode."
  (interactive)
  (if mode-line-format
      (setq mode-line-format nil)
    (setq mode-line-format ajv/pdf-tools/mode-modeline-format))
  (force-mode-line-update 1)		;Needed to actually see the change
  )

(defun ajv/pdf-tools/launch-file (returned-buffer)
  "Put an advice after find-file so that all PDFs are opened as a separate emacsclient (xdg-open) call."
  (when (with-current-buffer returned-buffer (derived-mode-p 'pdf-view-mode))
    (kill-buffer returned-buffer)
    (let ((process-connection-type nil))
      (start-process "*launch*" nil "xdg-open" (buffer-file-name returned-buffer))))
  )
