(provide 'ajv-misc)

(when (display-graphic-p)
  ;; Copy-pasting to-from other programs when in display graphics
  (setq  select-enable-clipboard t        ;copy/paste into other programs
	 interprogram-paste-function 'x-selection-value) ;from other progams
  )

(setq  next-line-add-newlines t
       sentence-end-double-space nil ;sentences don't have 2 spaces after '.'
       next-screen-context-lines 10   ;affects C-v and M-v
       indent-tabs-mode nil     ;Don't use tabs for indenting
       async-shell-command-buffer 'new-buffer         ;always use new buffer
       enable-recursive-minibuffers 1
       help-window-select t
       apropos-do-all t
       large-file-warning-threshold (* 100 1000000) ;Set to 100 MB.
       ;; max-specpdl-size 13000
       find-file-visit-truename t    ;follow symlinks to true targets
       vc-follow-symlinks t    ;follow symlinks that are in git repo
       time-stamp-pattern nil)            ;use local variables for time-stamps

;; Make Emacs remember the place in the buffer in previously opened files.
(save-place-mode 1)
(setq save-place-file (concat user-emacs-directory ".emacs-places")
      save-place-forget-unreadable-files nil)

(winner-mode 1)
(auto-compression-mode 1)
(delete-selection-mode t)
(global-auto-complete-mode 0)		;Do not auto-complete. Use company.
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; Misc diminish and delight settings
(diminish 'auto-revert-mode "")
(diminish 'auto-fill-mode "")
(diminish 'subword-mode "")
(diminish 'highlight-indentation-mode "")

(delight 'TeX-latex-mode "LaTeX" :major)
(diminish 'TeX-latex-mode "LaTeX")
(delight 'tex-mode "LaTeX" :major)
(diminish 'tex-mode "LaTeX")

(delight 'emacs-lisp-mode "ELisp" :major)
