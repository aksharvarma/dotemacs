(provide 'ajv-latex)

(setq-default TeX-engine 'xetex)

(setq LaTeX-command "latex -shell-escape --synctex=1"
      LaTeX-command-style '(("" "%(PDF)%(latex) -shell-escape %(file-line-error) %(extraopts) %S%(PDFout)"))
      TeX-save-query nil                ;Don't ask before saving .tex files
      ;; To make AUCTeX read/update on changes to .bib files.
      TeX-parse-self t ; Enable parse on load. [DISABLED]
      TeX-auto-save t ; Enable parse on save. [DISABLED]
      ;; TeX-force-default-mode t
      reftex-plug-into-AUCTeX t
      reftex-ref-macro-prompt nil)

(defun texcount ()
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

;; The idea for the following also comes from jrvarma's dot-emacs repo.
(setq TeX-view-program-selection
      (quote
       (((output-dvi has-no-display-manager)
	 "dvi2tty")
	((output-dvi style-pstricks)
	 "xdg-open")
	(output-dvi "xdvi")
	(output-pdf "PDF Tools")
	(output-html "xdg-open"))
       ))

(defun ajv/latex/toggle-reftex-cite-natbib-format ()
  "Toggles the reftex-cite-format variable between default and natbib"
  (interactive)
  (if (eq reftex-cite-format 'natbib)
      (setq reftex-cite-format 'default)
    (setq reftex-cite-format 'natbib)))
