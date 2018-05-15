;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Python-elpy mode
(provide 'ajv-python-elpy)

(defvar python-shell-interpreter-args)

(declare-function elpy-enable  "elpy")
(declare-function elpy-use-ipython  "elpy")
(declare-function dired-do-async-shell-command  "dired-aux")

(elpy-enable)
(elpy-use-ipython)
;; (setq python-shell-interpreter-args "--TerminalInteractiveShell.simple_prompt=True")
(setq python-shell-interpreter-args "")

(defvar elpy-rpc-backend)
(setq elpy-rpc-backend "jedi")

(defvar elpy-modules)
(when (require 'flycheck nil t)
 (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
 (add-hook 'elpy-mode-hook 'flycheck-mode))
