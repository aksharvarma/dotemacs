;; Transfer control over to my init.el
;; This file originally resides in ~/.emacs.d/site-lisp/ajv/redirect.el
;; That is symlinked to ~/.emacs.d/init.el
;; Which then makes the following call
(load "~/.emacs.d/site-lisp/ajv/ajv-init.el")

;; The symlink is because I want to version control this file.
;; My repo is inside ~/.emacs.d/site-lisp/ajv/
;; This is for historical reasons - this started as small scripts
;; When that became huge, I declared emacs bankruptcy
;; Started from scratch and added version control
;; Everything stayed in that folder though.
;; Now, I just keep it here since it doesn't hurt.
