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

(load "~/.emacs.d/site-lisp/ajv/ajv-settings.el")
;; (setq ajv/my-init-directory "~/.emacs.d/site-lisp/ajv/")
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

(use-package notmuch
  :commands notmuch)                      ;Don't use this yet.

(use-package ido
  :ensure t
  :demand
  :config
  (ido-mode t)
  (ido-everywhere t)
  (setq ido-enable-flex-matching t))

(when (not (featurep 'ido))
  (fset 'ido-completing-read 'completing-read)
  (fset 'ido-find-file 'find-file)
  (fset 'ido-switch-buffer 'switch-to-buffer)
  (fset 'ido-switch-buffer-other-window 'switch-to-buffer-other-window))


(use-package company
  :disabled
  :bind (("S-<tab>" . company-complete))
  :config (global-company-mode))

(use-package shell-pop
  :bind (("C-M-1" . shell-pop))
  :config
  (setq shell-pop-shell-type (quote ("ansi-term" "*ansi-term*"
                                     (lambda nil (ansi-term shell-pop-term-shell))))
        shell-pop-term-shell "/bin/bash")
  ;; need to do this manually or not picked up by `shell-pop'
  (shell-pop--set-shell-type 'shell-pop-shell-type shell-pop-shell-type))

(use-package python
  ;; :disabled
  :mode ("\\.py\\'" . python-mode)
  :defer 2
  :commands python-mode
  :config
  (elpy-enable)
  (setq python-shell-interpreter "ipython"
        python-shell-interpreter-args "--TerminalInteractiveShell.simple_prompt=True"
        elpy-rpc-backend "jedi")
  (use-package flycheck
    :config
    (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
    (add-hook 'elpy-mode-hook 'flycheck-mode))
  )

(use-package move-text :config (move-text-default-bindings))

(use-package smex
  :bind (("M-x" . smex))
  :config (smex-initialize))

(use-package key-chord
  :demand
  :disabled
  :bind (("C-c C-k C-t" . key-chord-mode))
  :config (key-chord-mode 1)
  )

(use-package powerline :after (ajv-visual ajv-modeline) :config (powerline-default-theme))

(use-package ajv-pdf
  :init
  (use-package pdf-tools
    :defer 2
    :magic ("%PDF" . pdf-view-mode)
    :pin manual
    :config (pdf-tools-install))
  :bind (:map pdf-view-mode-map ("q" . delete-frame))
  :config (setq pdf-view-resize-factor 1.05)
  :hook ((pdf-view-mode . ajv/pdf-view-move-modeline-to-top))
  )

(use-package ajv-magit
  :init (use-package magit :commands (magit-status magit-mode))
  :commands (magit-status magit-mode)
  :bind (("<f3>" . magit-status)
         :map magit-status-mode-map
         ("q" . ajv/magit-kill-buffers)))

(use-package ajv-dired
  :demand
  :init (use-package dired)
  :config
  (setq dired-dwim-target t                     ;default copy to other window
        dired-listing-switches "-a -l -L -h --group-directories-first --classify")
  (put 'dired-find-alternate-file 'disabled nil) ;allow 'a' in dired
  :bind  (:map dired-mode-map ("s". ajv/dired-sort-criteria))
  :hook ((dired-mode . ajv/dired-set-default-sorting)
	 (dired-mode . ajv/dired-hide-details-omit-hidden-files))
  )

(use-package ajv-my-functions
  :demand
  :bind
  (("s-8" . ajv/switch-buffer-scratch)
   ("s-~" . ajv/open-home-in-dired)
   ("s-`" . ajv/open-symlink-folder-in-dired)
   ("s-p" . ajv/mypaths)
   ("C-c w c". ajv/window-config)
   ("C-c s u" . ajv/reopen-file-with-sudo)
   ("C-c C-d C-b" . ajv/delete-backup-files)
   ("%" . ajv/match-paren)
   ("s-o" . ajv/close-other-buffer)
   ("s-w" . kill-this-buffer)
   ("C-x z" .  bury-buffer)
   ("s-<tab>" . other-window)
   ("s-b" . ido-switch-buffer)
   ("s-B" . ido-switch-buffer-other-window)
   ("s-s" . save-buffer)
   ("s-f" . ido-find-file)
   ("s-g" . keyboard-quit)
   :map dired-mode-map
   ("l" . ajv/dired-launch-file))
  :config
  (when window-system
    (global-set-key (kbd "C-x C-c") 'ajv/ask-before-closing))
  (advice-add 'revert-buffer :around #'yes-or-no-p->-y-or-n-p)
  :hook
  ((prog-mode . ajv/hideshow-setup)
   (emacs-startup . ajv/measure-loading-time))
  )

(use-package ajv-misc :defer 1 :init (setq inhibit-startup-message t))

(use-package ajv-visual)

(use-package ajv-modeline :defer 1)
