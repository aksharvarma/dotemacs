(provide 'ajv-misc)

(setq org-startup-truncated 'nil
      LaTeX-command "latex -shell-escape"
      org-todo-keywords '((sequence "TODO(t)" "|" "IN-PROGRESS(i)" "|" "DONE(d)"))
      next-line-add-newlines t
      async-shell-command-buffer 'new-buffer         ;always use new buffer
      select-enable-clipboard t        ;copy/paste into other programs
      interprogram-paste-function 'x-selection-value ;from other progams
      ;; To make AUCTeX read/update on changes to .bib files.
      TeX-parse-self nil ; Enable parse on load. [DISABLED]
      TeX-auto-save nil ; Enable parse on save. [DISABLED]
      indent-tabs-mode nil)     ;Don't use tabs for indenting

(add-hook 'before-save-hook 'time-stamp) ;set time-stamp before saving file
(setq time-stamp-pattern nil)            ;use local variables for time-stamps

(setq custom-file (concat ajv/my-init-directory "ajv-customizations.el"))
(load custom-file)

(winner-mode 1)
(auto-compression-mode 1)
(delete-selection-mode t)
(global-auto-complete-mode t)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

