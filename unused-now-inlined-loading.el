;;;>>>
;;;For using package manager.
(require 'package)
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
(require 'jrv-mypaths)
;; (require 'dired-details+)
(require 'dired-sort-map)
(require 'cl)
(require 'init_python_elpy nil nil)
(require 'notmuch)
;;;}}}


