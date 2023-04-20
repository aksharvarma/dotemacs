;; Load the custom-file which only has package-selected-packages
;; This will later be used to ensure that all of those have been installed.
(setq custom-file (concat ajv/settings/my-init-directory ajv/settings/custom-file-name))
(load custom-file)

;;;This adds all elpa folder packages to the load path,
;;;so that .el files for those packages, are visible during initialization.
(let ((default-directory (concat user-emacs-directory "elpa/")))
  (normal-top-level-add-subdirs-to-load-path))

(require 'package)
;; To prevent initialising twice
(setq package-enable-at-startup nil)
;;Don't add (package-initialize) to .emacs.
(setq package--init-file-ensured t)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ("melpa-stable" . "https://stable.melpa.org/packages/")
			 ("melpa" . "http://melpa.org/packages/")))
(setq package-menu-hide-low-priority t)
(setq package-archive-priorities
      '(("MELPA Stable" . 10)
	("GNU ELPA"     . 5)
	("MELPA"        . 0)))

(package-initialize t)

;; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

;; install the missing packages
(dolist (package package-selected-packages)
  (unless (package-installed-p package)
    (package-install package)))


;; Everything after this point will be via use-package
;; Get use-package and its dependencies
(eval-when-compile
  ;; (add-to-list 'load-path "")
  (require 'use-package))
