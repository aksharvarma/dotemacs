;; Increase garbage collection threshold during startup and brings it back to reasonable values at the end.
(setq gc-cons-threshold (* 500 1024 1024)) ;500MB
(setq gc-cons-percentage 0.95)
(add-hook 'after-init-hook (lambda ()
			     ;; garbage collection happens more frequently but in less time.
			     (setq gc-cons-threshold (* 2 1024 1024))
			     (setq gc-cons-percentage 0.1)))

;; Stop some visual elemnts very early on.
;; These are put here mainly for speeding up things
;; I am not really sure if it does help actually.
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(setq inhibit-startup-screen t)
(setq truncate-lines nil)
(mouse-avoidance-mode 'banish)
(setq mouse-avoidance-banish-position '((frame-or-window . frame)
					(side . right)
					(side-pos . 0)
					(top-or-bottom . bottom)
					(top-or-bottom-pos . 0)))

(setq initial-major-mode 'poly-org-mode)
(setq initial-scratch-message
      (with-temp-buffer
	(insert-file-contents
	 (concat user-emacs-directory "site-lisp/ajv/scratch-buffer-init-content.org"))
	(buffer-string)))

;; Load basic settings directly.
(setq ajv/my-settings-file-name (concat user-emacs-directory "site-lisp/ajv/ajv-settings.el"))
(load ajv/my-settings-file-name)

;;
;; ;; Load the custom-file which only has package-selected-packages
;; ;; This will later be used to ensure that all of those have been installed.
;; (setq custom-file (concat ajv/settings/my-init-directory ajv/settings/custom-file-name))
;; (load custom-file)
;;

;;;This adds site-lisp and its subdirectories to the load path,
;;;so that .el files there, are visible while initialization.
(let ((default-directory ajv/settings/my-init-directory))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))
