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

;; Load basic settings directly.
(load "~/.emacs.d/site-lisp/ajv/ajv-settings.el")
(load "~/.emacs.d/site-lisp/ajv/ajv-sensitive-settings.el")

;;;This adds site-lisp and its subdirectories to the load path,
;;;so that .el files there, are visible while initialization.
(let ((default-directory ajv/my-init-directory))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

(require 'package)
(setq package-enable-at-startup nil   ; To prevent initialising twice
      package--init-file-ensured t     ;Don't add (package-initialize) to .emacs.
      package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/"))
      package-archive-priorities
      '(("MELPA Stable" . 10)
        ("GNU ELPA"     . 5)
        ("MELPA"        . 0)))
(package-initialize)


;; Get use-package and its dependencies
(require 'use-package)
(use-package diminish)
(use-package bind-key)

;; Start loading up other things
(use-package cl)

(use-package notmuch
  :commands notmuch
  :config
  (setq notmuch-search-oldest-first nil
	mm-text-html-renderer 'w3m
	notmuch-multipart/alternative-discouraged '("text/plain" "text/html"))
  (setq notmuch-saved-searches
	 (quote
	  ((:name "main-gmail" :query "path:main-gmail/**" :key "m")
	   (:name "neu-email" :query "path:neu-email/**" :key "n")
	   (:name "unread" :query "tag:unread" :key "u")
	   (:name "all mail" :query "*" :key "a")
	   (:name "sent" :query "tag:sent" :key "t")
	   (:name "flagged" :query "tag:flagged" :key "f")
	   (:name "drafts" :query "tag:draft" :key "d"))))
  )

(use-package ido :demand
  :ensure t
  :config
  (ido-mode t)
  (ido-everywhere t)
  (setq ido-enable-flex-matching t)
  (add-to-list 'ido-ignore-buffers "^.*\\.pdf$"))

(use-package ido-completing-read+ :disabled :demand :config (ido-ubiquitous-mode 0))

(use-package ido-yes-or-no :disabled :demand :config (ido-yes-or-no-mode 0))

(use-package icomplete :disabled :demand :config (icomplete-mode 0))

(when (not (featurep 'ido))
  (fset 'ido-completing-read 'completing-read)
  (fset 'ido-find-file 'find-file)
  (fset 'ido-switch-buffer 'switch-to-buffer)
  (fset 'ido-switch-buffer-other-window 'switch-to-buffer-other-window))

(use-package company :disabled
  :bind (("S-<tab>" . company-complete))
  :config (global-company-mode))

(use-package god-mode
  :demand
  :bind (("<escape>" . god-mode-all)
	 (:map god-local-mode-map
	       ("." . repeat)
	       ("i" . god-mode-all)
	       ("<escape>" . nil)))
  :hook ((god-mode-enabled . ajv/god-update-cursor)
	 (god-mode-disabled . ajv/god-update-cursor))
  :config
  (use-package ajv-god)
  (add-to-list 'god-exempt-major-modes 'org-agenda-mode)
  (add-to-list 'god-exempt-major-modes 'elfeed-search-mode)
  (god-mode-all)
  )

(use-package god-mode-isearch
  :after (god-mode)
  :bind ((:map isearch-mode-map ("<escape>" . god-mode-isearch-activate))
	 (:map god-mode-isearch-map ("<escape>" . god-mode-isearch-disable)))
  )

(use-package shell-pop
  :bind (("C-M-1" . shell-pop))
  :config
  (setq shell-pop-shell-type (quote ("ansi-term" "*ansi-term*"
                                     (lambda nil (ansi-term shell-pop-term-shell))))
        shell-pop-term-shell "/bin/bash")
  ;; need to do this manually or not picked up by `shell-pop'
  (shell-pop--set-shell-type 'shell-pop-shell-type shell-pop-shell-type))

(use-package python :defer 2
  :mode ("\\.py\\'" . python-mode)
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
  ;; :bind (:map elpy-mode-map
  ;; 	      ("<f5>" . elpy-shell-region-or-buffer))
  )

(use-package move-text :config (move-text-default-bindings))

(use-package smex
  :bind (("M-x" . smex))
  :config (smex-initialize))

(use-package key-chord :demand :disabled
  :bind (("C-c C-k C-t" . key-chord-mode))
  :config (key-chord-mode 1)
  )

(use-package powerline
  :disabled
  :after (ajv-visual ajv-modeline) ;; :config (powerline-default-theme)
  )

(use-package smart-mode-line
  :disabled
  :after (ajv-visual ajv-modeline)
  :config
  (setq sml/theme 'dark)
  (setq sml/no-confirm-load-theme t)
  (sml/setup)
  )

(use-package smart-mode-line-powerline-theme
   ;; :ensure t
  :disabled
  :after powerline
  :after smart-mode-line
  :config
    (sml/setup)
    (sml/apply-theme 'powerline)
    )

(use-package moody
  :after (ajv-visual smart-mode-line)
  :disabled
  :config
  (setq x-underline-at-descent-line t
	moody-mode-line-height 14)
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode)
  )

(use-package minions
  :disabled
  :bind (("M-S-RET" . minions-minor-modes-menu))
  :config (minions-mode 1))

(use-package pdf-tools :defer 2 :magic ("%PDF" . pdf-view-mode) :pin manual
  :config
  (pdf-tools-install)
  (use-package ajv-pdf :demand
    :bind (:map pdf-view-mode-map
		("q" . delete-frame)
		("M-m" . ajv/pdf-view-toggle-modeline)
		("M-i" . pdf-view-midnight-minor-mode))
    :config (setq pdf-view-resize-factor 1.05
		  auto-revert-interval 0.1
		  auto-revert-verbose nil)
    :hook ((pdf-view-mode . ajv/pdf-view-save-disable-modeline-format)
	   (pdf-view-mode . ajv/pdf-view-disable-linum-mode)
	   (pdf-view-mode . auto-revert-mode)
	   (pdf-view-mode . pdf-misc-size-indication-minor-mode)))
  )

(use-package magit
  :bind (("<f3>" . magit-status))
  :config
  (use-package ajv-magit :demand
    :bind (:map magit-status-mode-map
		([remap magit-mode-bury-buffer] . ajv/magit-kill-buffers)))
  (setq magit-completing-read-function 'magit-ido-completing-read)
  )

(use-package dired :demand
  :config
  (setq dired-dwim-target t                     ;default copy to other window
        dired-listing-switches "-a -l -L -h --group-directories-first --classify"
	dired-recursive-copies 'always)
  (put 'dired-find-alternate-file 'disabled nil) ;allow 'a' in dired
  (use-package ajv-dired
    :bind  (:map dired-mode-map
		 ("s". ajv/dired-sort-criteria)
		 ("l" . ajv/dired-launch-file)
		 ("C-c C-d C-b" . ajv/delete-backup-files))
    :hook ((dired-mode . ajv/dired-set-default-sorting)
	   (dired-mode . ajv/dired-hide-details-omit-hidden-files)))
  )

(use-package ajv-my-functions :demand
  :bind
  (("s-8" . ajv/switch-buffer-scratch)
   ("s-~" . ajv/open-home-in-dired)
   ("s-`" . ajv/open-symlink-folder-in-dired)
   ("s-p" . ajv/mypaths)
   ("s-P" . ajv/mypaths-other-window)
   ("C-c w c". ajv/window-config)
   ("C-c s u" . ajv/reopen-file-with-sudo)
   ("%" . ajv/match-paren)
   ("s-w" . ajv/kill-this-buffer)
   ("s-o" . ajv/kill-other-buffer)
   ("C-x z" .  bury-buffer)
   ("s-<tab>" . other-window)
   ("s-b" . ido-switch-buffer)
   ("s-B" . ido-switch-buffer-other-window)
   ("s-s" . save-buffer)
   ("s-f" . ido-find-file)
   ("s-g" . keyboard-quit))
  :config
  (eval-after-load 'latex
    '(define-key LaTeX-mode-map (kbd "<f5>") 'TeX-command-master))
  (when window-system
    (global-set-key (kbd "C-x C-c") 'ajv/ask-before-closing))
  (advice-add 'revert-buffer :around #'yes-or-no-p->-y-or-n-p)
  :hook
  ((prog-mode . ajv/hideshow-setup)
   (emacs-startup . ajv/measure-loading-time)
   (before-save . ajv/delete-trailing-whitespace))
  )

(use-package ajv-org
  :demand
  :bind
  (("s-a" . org-agenda))
  ;; :config
  ;; (setq org-agenda-custom-commands
  ;; 	'(("n" "Agenda and all TODOs"
  ;; 	   ((agenda #1="")
  ;; 	    (alltodo #1#)))
  ;; 	  ("l" "Show agenda with log and time report"
  ;; 	       ((agenda "" (org-agenda-log-mode))))))
  :hook ((after-init . org-agenda-list))
  )

(use-package elfeed :hook ((elfeed-search-mode . toggle-truncate-lines)))

;; http://pragmaticemacs.com/emacs/read-your-rss-feeds-in-emacs-with-elfeed
(use-package elfeed-org
  :config
  (elfeed-org)
  (setq rmh-elfeed-org-files ajv/my-elfeed-org-file)
  )

(use-package ajv-misc
  :defer 1
  :init (setq inhibit-startup-message t)
  :hook ((before-save . time-stamp))
  )

(use-package ajv-visual)
  ;; All of the following is for moody
  ;; :config
  ;; (let ((line (face-attribute 'mode-line :underline)))
  ;;   (set-face-attribute 'mode-line          nil :overline   line)
  ;;   (set-face-attribute 'mode-line-inactive nil :overline   line)
  ;;   (set-face-attribute 'mode-line-inactive nil :underline  line)
  ;;   (set-face-attribute 'mode-line          nil :box        nil)
  ;;   (set-face-attribute 'mode-line-inactive nil :box        nil)
  ;;   (set-face-attribute 'mode-line-inactive nil :background "#f9f2d9")))

(use-package ajv-modeline
  ;; :defer 1
  )
