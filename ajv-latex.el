(provide 'ajv-latex)

(setq LaTeX-command "latex -shell-escape"
      TeX-save-query nil                ;Don't ask before saving .tex files
      ;; To make AUCTeX read/update on changes to .bib files.
      TeX-parse-self nil ; Enable parse on load. [DISABLED]
      TeX-auto-save nil ; Enable parse on save. [DISABLED]
      reftex-plug-into-AUCTeX t)
