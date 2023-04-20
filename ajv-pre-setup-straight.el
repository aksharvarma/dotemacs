;; ;;
;; ;;;This adds all elpa folder packages to the load path,
;; ;;;so that .el files for those packages, are visible during initialization.
;; (let ((default-directory (concat user-emacs-directory "elpa/")))
;;   (normal-top-level-add-subdirs-to-load-path))
;; ;;
;;;; ;;;; ;;;; ;;;; ;;;; ;;;; ;;;; ;;;; ;;;; ;;;; ;;;; ;;;; ;;;; ;;;; ;;;; ;;;;
;;
;; I will use `straight-rebuild-package' or `straight-rebuild-all' to do rebuilds.
;; Hopefully this will save some time during init.
;; Maybe `straight-check-all' is a better function to use for this.
;;
;; (setq straight-check-for-modifications nil)
;;
;; Copied from: https://github.com/radian-software/straight.el#getting-started
;;

(setq straight-check-for-modifications '(check-on-save find-when-checking))
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))


(straight-use-package 'use-package)
(setq straight-use-package-by-default t)
