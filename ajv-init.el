;; My init. Control transfers from .emacs.d/init.el to here.

;;;First I set up package manager and (require ...) things.
;; (load "init_loading")
;;;>>>
;;;For using package manager.
(require 'package)
;; ;; ;; ;; ;; ;; ;; 
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))
;;;}}}

;;;>>>
;;;Interactively do things (Ido):->
(require 'ido)
(ido-mode t)
;;;}}}

;;;>>>
;;;Now the requires:->
;; (require 'jrv-launcher)                 ; Made obsolete by gmrun 
;; (require 'dired-details+)
(require 'cl)                           ;Common lisp
(require 'jrv-mypaths)                  ;Eventually replace with bookmarsk
(require 'dired-sort-map)               ;Sorting nicely in dired
(require 'ajv-python-elpy nil nil)     ;Python REPL (I think)
(require 'notmuch)                      ;Don't use this yet.
;;;}}}

;; Save customization from the menu in the following file.
(setq custom-file "ajv-customizations.el")
;; And load that file.
(load custom-file)

;;;The dired sorting thing. Figure out what's different from dired-sort-map
(load "ajv-dired-sorting")

;;;Now defining some functions.
(load "ajv-my-functions")

;;;Now for the miscellenous stuff.
(load "ajv-misc")

;;;Now the key-bindings.
(load "ajv-key-bindings")

;;; Visual and look
(load "ajv-visual")

