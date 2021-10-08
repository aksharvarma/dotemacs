(provide 'ajv-misc)

(when (display-graphic-p)
  ;; Copy-pasting to-from other programs when in display graphics
  (setq  select-enable-clipboard t)        ;copy/paste into other programs
  (setq interprogram-paste-function 'x-selection-value) ;from other progams
  ;; Contents of clipboard (from outside) preserved before kills in emacs
  ;; So M-y will get those clipboard entries.
  (setq save-interprogram-paste-before-kill t)
  )

(setq kill-read-only-ok t)

(setq  next-line-add-newlines t)
(setq sentence-end-double-space nil)	;sentences don't have 2 spaces after '.'
(setq next-screen-context-lines 10)	;affects C-v and M-v
(setq indent-tabs-mode nil)		;Don't use tabs for indenting
(setq async-shell-command-buffer 'new-buffer) ;always use new buffer
(setq enable-recursive-minibuffers 1)
(setq help-window-select t)
(setq apropos-do-all t)
(setq large-file-warning-threshold (* 100 1000000)) ;Set to 100 MB.
;; (setq max-specpdl-size 13000)
(setq find-file-visit-truename t)	;follow symlinks to true targets
(setq vc-follow-symlinks t)		;follow symlinks that are in git repo
(setq time-stamp-pattern nil)		;use local variables for time-stamps

;; Make Emacs remember the place in the buffer in previously opened files.
(save-place-mode 1)
(setq save-place-file (concat user-emacs-directory ".emacs-places"))
(setq save-place-forget-unreadable-files nil)

(winner-mode 1)
(auto-compression-mode 1)
(delete-selection-mode t)
(global-auto-complete-mode 0)		;Do not auto-complete. Use company.
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'list-timers 'disabled nil)
