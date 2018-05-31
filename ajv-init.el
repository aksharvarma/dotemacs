;; Control transfers from .emacs.d/init.el to here.

;; https://www.emacswiki.org/emacs/ProfileDotEmacs
;; Go to the `profile-dotemacs.el` file and modify the file name to what you want. Then run the following to profile it nicely.
;; Note: Only top level sexps are profiled, so you might want to make sure that what you really want to look at is actually at the top of the list.
;; emacs -Q -l ~/.emacs.d/site-lisp/ajv/profile-dotemacs.el -f profile-dotemacs

(setq ajv/my-init-directory "~/.emacs.d/site-lisp/ajv/")
;;;This adds site-lisp and its subdirectories to the load path,
;;;so that .el files there, are visible while initialization.
(let ((default-directory ajv/my-init-directory))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

(require 'package)
(setq package-enable-at-startup nil   ; To prevent initialising twice
      package--init-file-ensured t     ;Don't add (package-initialize) to .emacs.
      package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)

;; Supposed to make loading things faster. These lines need to be here.n
(setq gc-cons-threshold (* 500 1024 1024) ;500MB
      gc-cons-percentage 0.8)
;; Keeping corresponding ending code here as well, so I don't forget.
(add-hook 'emacs-startup-hook (lambda () (setq gc-cons-threshold (* 2 1024 1024)
                                               gc-cons-percentage 0.1)))

;; Get use-package and its dependencies
(require 'use-package)
(use-package diminish)
(use-package bind-key)

;; Start loading up other things that I want
(use-package cl)
(use-package jrv-mypaths)                  ;Eventually replace with bookmarsk
(use-package notmuch
  :commands notmuch)                      ;Don't use this yet.

(use-package ido
  :ensure t
  :demand
  :config
  (ido-mode t)
  (setq ido-enable-flex-matching t))

(use-package python
  ;; :disabled
  :mode ("\\.py\\'" . python-mode)
  :defer 2
  :commands python-mode
  :config
  (elpy-enable)
  (setq python-shell-interpreter "ipython")
  (setq python-shell-interpreter-args "--TerminalInteractiveShell.simple_prompt=True")
  (setq elpy-rpc-backend "jedi")
  (use-package flycheck
    :config
    (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
    (add-hook 'elpy-mode-hook 'flycheck-mode))
  )

(use-package pdf-tools
  :defer 2
  :magic ("%PDF" . pdf-view-mode)
  :config
  (pdf-tools-install))

(use-package smex
  :ensure t
  :bind (("M-x" . smex))
  :config (smex-initialize))

(use-package magit
  :commands (magit-status magit-mode)
  :bind (("C-x g" . magit-status)))

(use-package dired
  :demand
  :config
  ;; default to other dired window for copy and such things
  (setq dired-dwim-target t)
  ;; This is what lets 'a' work in the dired mode
  (put 'dired-find-alternate-file 'disabled nil)
  ;; Making directories come first in dired mode
  (setq dired-listing-switches "-a -l -L -h --group-directories-first")
  ;; hide-details and omit-mode hooked onto dired-mode
  (add-hook 'dired-mode-hook (lambda ()
                               ;; Hiding all the details (`ls -l` things)
                               (dired-hide-details-mode)
                               ;; Hiding hidden files in dired mode
                               (use-package dired-x)
                               (setq dired-omit-files "^\\...+$")
                               (dired-omit-mode 1)))
  ;;The dired sorting thing. Figure out what's different from dired-sort-map
  (use-package ajv-dired-sorting)
  (use-package dired-sort-map)               ;Sorting nicely in dired
  )

;;;Now defining some functions.
(use-package ajv-my-functions
  :demand
  :bind
  (("C-*" . switch-buffer-scratch)
   ("C-x c" . close-other-buffer)
   ("C-x x" . kill-this-buffer)
   ("C-x z" .  bury-buffer)
   ("C-<tab>" . other-window)
   ("%" . match-paren)
   ("C-`" . open-home-in-dired)
   ("C-~" . open-home-in-dired)
   ("C-c C-d C-b" . delete-backup-files)
   ("s-b" . ido-switch-buffer)
   ("C-C w c". ajv-window-config)
   ("C-c s u" . reopen-file-with-sudo)
   :map dired-mode-map
   ("l" . dired-launch-file))
  :config
  (when window-system
    (global-set-key (kbd "C-x C-c") 'ask-before-closing))
  (add-hook 'prog-mode-hook 'hideshow-setup)
  (add-hook 'emacs-startup-hook 'measure-loading-time)
  (advice-add 'revert-buffer :around #'yes-or-no-p->-y-or-n-p)
  )

;;;Now for the miscellenous stuff.
(use-package ajv-misc)

