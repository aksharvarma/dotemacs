;; Control transfers from .emacs.d/init.el to here.

;; https://www.emacswiki.org/emacs/ProfileDotEmacs
;; Go to the `profile-dotemacs.el` file and modify the file name to what you want. Then run the following to profile it nicely.
;; Note: Only top level sexps are profiled, so you might want to make sure that what you really want to look at is actually at the top of the list.
;; emacs -Q -l ~/.emacs.d/site-lisp/profile-dotemacs.el -f profile-dotemacs

;; Increase garbage collection threshold during startup and brings it back to reasonable values at the end.
(setq gc-cons-threshold (* 500 1024 1024) ;500MB
      gc-cons-percentage 0.8)
(add-hook 'emacs-startup-hook (lambda () (setq gc-cons-threshold (* 2 1024 1024)
                                               gc-cons-percentage 0.1)))

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


;; Get use-package and its dependencies
(require 'use-package)
(use-package diminish)
(use-package bind-key)

;; Start loading up other things
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

(use-package shell-pop
  :bind (("C-M-1" . shell-pop))
  :config
  (setq shell-pop-shell-type (quote ("ansi-term" "*ansi-term*" (lambda nil (ansi-term shell-pop-term-shell)))))
  (setq shell-pop-term-shell "/bin/bash")
  ;; need to do this manually or not picked up by `shell-pop'
  (shell-pop--set-shell-type 'shell-pop-shell-type shell-pop-shell-type))

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
  :pin manual
  :config
  (pdf-tools-install)
  (setq pdf-view-resize-factor 1.05))

(use-package smex
  ;; :demand
  :bind (("M-x" . smex))
  :config (smex-initialize))

(use-package magit
  :commands (magit-status magit-mode)
  :bind (("C-x g" . magit-status)))

(use-package ajv-dired
  :demand
  :init (use-package dired)
  :config
  (setq dired-dwim-target t)                     ;default copy to other window
  (put 'dired-find-alternate-file 'disabled nil) ;allow 'a' in dired
  (setq dired-listing-switches "-a -l -L -h --group-directories-first")
  :bind  (:map dired-mode-map
               ("s". dired-sort-criteria))
  :hook
  ((dired-mode . ajv/dired-set-default-sorting)
   (dired-mode . ajv/dired-hide-details-omit-hidden-files))
  )

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
   ("s-B" . ido-switch-buffer-other-window)
   ("C-C w c". ajv/window-config)
   ("C-c s u" . reopen-file-with-sudo)
   ("<f5>" . save-buffer)
   :map dired-mode-map
   ("l" . dired-launch-file))
  :config
  (when window-system
    (global-set-key (kbd "C-x C-c") 'ask-before-closing))
  (advice-add 'revert-buffer :around #'yes-or-no-p->-y-or-n-p)
  :hook
  ((prog-mode . hideshow-setup)
   (emacs-startup . measure-loading-time))
  )

(use-package ajv-misc
  :defer 1
  :init
  (setq inhibit-startup-message t)
  )

(use-package ajv-visual)

(use-package ajv-modeline)
