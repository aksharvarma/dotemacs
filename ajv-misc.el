(provide 'ajv-misc)

(setq LaTeX-command "latex -shell-escape"
      TeX-save-query nil                ;Don't ask before saving .tex files
      next-line-add-newlines t
      async-shell-command-buffer 'new-buffer         ;always use new buffer
      select-enable-clipboard t        ;copy/paste into other programs
      interprogram-paste-function 'x-selection-value ;from other progams
      ;; To make AUCTeX read/update on changes to .bib files.
      TeX-parse-self nil ; Enable parse on load. [DISABLED]
      TeX-auto-save nil ; Enable parse on save. [DISABLED]
      indent-tabs-mode nil     ;Don't use tabs for indenting
      enable-recursive-minibuffers 1
      sentence-end-double-space nil ;sentences don't have 2 spaces after '.'
      find-file-visit-truename t    ;follow symlinks to true targets
      vc-follow-symlinks t    ;follow symlinks that are in git repo
      time-stamp-pattern nil)            ;use local variables for time-stamps

(setq custom-file (concat ajv/my-init-directory ajv/custom-file-name))
(load custom-file)

(winner-mode 1)
(subword-mode)				; Allows moving through camelCasedWords
(auto-compression-mode 1)
(delete-selection-mode t)
(global-auto-complete-mode t)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
