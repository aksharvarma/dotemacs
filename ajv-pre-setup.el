;; Increase garbage collection threshold during startup and brings it back to reasonable values at the end.
(setq gc-cons-threshold (* 500 1024 1024) ;500MB
      gc-cons-percentage 0.8)
(add-hook 'after-init-hook (lambda () (setq gc-cons-threshold (* 100 1024 1024)
					    gc-cons-percentage 0.1)))

;; Stop some visual elemnts very early on.
;; These are put here mainly for speeding up things
;; I am not really sure if it does help actually.
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(setq inhibit-startup-screen t)

;; Load basic settings directly.
(setq ajv/my-settings-file-name (concat user-emacs-directory "site-lisp/ajv/ajv-settings.el"))
(load ajv/my-settings-file-name)
;; Load the custom-file which only has package-selected-packages
;; This will later be used to ensure that all of those have been installed.
(setq custom-file (concat ajv/settings/my-init-directory ajv/settings/custom-file-name))
(load custom-file)

;;;This adds site-lisp and its subdirectories to the load path,
;;;so that .el files there, are visible while initialization.
(let ((default-directory ajv/settings/my-init-directory))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

(require 'package)
(setq package-enable-at-startup nil   ; To prevent initialising twice
      package--init-file-ensured t     ;Don't add (package-initialize) to .emacs.
      package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("melpa" . "http://melpa.org/packages/"))
      package-menu-hide-low-priority t
      package-archive-priorities
      '(("MELPA Stable" . 10)
        ("GNU ELPA"     . 5)
        ("MELPA"        . 0)))
(package-initialize)

;; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

;; install the missing packages
(dolist (package package-selected-packages)
  (unless (package-installed-p package)
    (package-install package)))
