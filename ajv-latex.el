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


(defvar-local ajv/latex/use-cleveref nil
  "A variable that is used by the `ajv/latex/reftex-reference-cleveref-wrapper'
function to determine whether to use the cleveref package's reference system of the default")

(defun ajv/latex/reftex-reference-cleveref-wrapper ()
  "Automatically choose reftex's preferred reference format between \\cref and the default based on the file local variable `ajv/latex/use-cleveref' which defaults to nil. So please set that variable to true if you wish to use cleveref by default."
  (interactive)
  (if ajv/latex/use-cleveref
      (call-interactively 'reftex-cleveref-cref)
    (call-interactively 'reftex-reference)))
