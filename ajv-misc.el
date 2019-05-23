(provide 'ajv-misc)

(when (display-graphic-p)
  ;; Copy-pasting to-from other programs when in display graphics
  (setq  select-enable-clipboard t        ;copy/paste into other programs
	 interprogram-paste-function 'x-selection-value) ;from other progams
  )

(setq  next-line-add-newlines t
       sentence-end-double-space nil ;sentences don't have 2 spaces after '.'
       indent-tabs-mode nil     ;Don't use tabs for indenting
       async-shell-command-buffer 'new-buffer         ;always use new buffer
       enable-recursive-minibuffers 1
       help-window-select t
       apropos-do-all t
       large-file-warning-threshold 50000000 ;Set to 50 MB.
       ;; max-specpdl-size 13000
       find-file-visit-truename t    ;follow symlinks to true targets
       vc-follow-symlinks t    ;follow symlinks that are in git repo
       time-stamp-pattern nil)            ;use local variables for time-stamps

(setq custom-file (concat ajv/my-init-directory ajv/custom-file-name))
(load custom-file)

;; Make Emacs remember the place in the buffer in previously opened files.
(save-place-mode 1)
(setq save-place-file (concat user-emacs-directory ".emacs-places")
      save-place-forget-unreadable-files nil)

(winner-mode 1)
(subword-mode)				; Allows moving through camelCasedWords
(auto-compression-mode 1)
(delete-selection-mode t)
(global-auto-complete-mode 0)		;Do not auto-complete. Use company.
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
