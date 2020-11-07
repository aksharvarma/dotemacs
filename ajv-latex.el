(provide 'ajv-latex)

(defun ajv/latex/count-words ()
  "Counts the number of words in the .tex file using the texcount command.

Taken from jrv-auctex-config.el in the repo: https://github.com/jrvarma/dot-emacs/"
  (interactive)
  (let* ((this-file (buffer-file-name))
	 (word-count
          (with-output-to-string
            (with-current-buffer standard-output
              (call-process "texcount" nil t nil "-1" this-file)))))
    (message word-count))
  )

(defun ajv/latex/toggle-reftex-cite-natbib-format ()
  "Toggles the reftex-cite-format variable between default and natbib"
  (interactive)
  (if (eq reftex-cite-format 'natbib)
      (setq reftex-cite-format 'default)
    (setq reftex-cite-format 'natbib)))
