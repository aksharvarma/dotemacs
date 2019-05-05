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
  :commands notmuch notmuch-jump-search notmuch-search
  :config
  (use-package ajv-notmuch :demand
    :bind ((:map notmuch-show-mode-map
		 ("u" . ajv/notmuch-show-toggle-unread))
	   (:map notmuch-search-mode-map
		 ("u" . ajv/notmuch-search-toggle-unread)
		 ("g" . notmuch-poll-and-refresh-this-buffer))
	   (:map notmuch-hello-mode-map
		 ("g" . notmuch-poll-and-refresh-this-buffer)))
    :hook (notmuch-hello-refresh . ajv/notmuch-set-initial-cursor-position))
  )

(use-package ido :demand
  :ensure t
  :config
  (ido-mode t)
  (ido-everywhere)
  (setq ido-enable-flex-matching t
	ido-auto-merge-work-directories-length -1)
  (use-package flx-ido
    :demand
    :config
    (flx-ido-mode 1)
    ;; Set ido-use-faces to nil if you want to see flx colors instead of ido
    (setq ido-use-faces t)
    ;; Uncomment line below if the faces don't gel nicely
    ;; (setq flx-ido-use-faces nil)
    )
  (add-to-list 'ido-ignore-buffers "^.*\\.pdf$"))


(when (not (featurep 'ido))
  (fset 'ido-completing-read 'completing-read)
  (fset 'ido-find-file 'find-file)
  (fset 'ido-switch-buffer 'switch-to-buffer)
  (fset 'ido-switch-buffer-other-window 'switch-to-buffer-other-window))



(use-package yasnippet
  :init (setq yas-snippet-dirs '(ajv/yasnippets-directory))
  :config
  (yas-global-mode 1)
  (yas-reload-all)
  (setq yas-prompt-functions
	'(yas-maybe-ido-prompt yas-completing-prompt yas-dropdown-prompt yas-no-prompt))
  :bind ((:map yas-minor-mode-map
	       ("C-c y" . #'yas-expand)))
  )

(use-package ajv-ibuffer
  :bind
  (("C-x C-b" . ibuffer)
   ("<f9>" . ibuffer)
   ("C-x C-S-b" . ibuffer-other-window)
   (:map ibuffer-mode-map
	 ("<up>" . ibuffer-previous-line)
	 ("<down>" . ibuffer-next-line)))
  :config
  (use-package ibuffer-vc :demand)
  :hook
  (;; (ibuffer-mode . ajv/group-ibuffer-by-vc)
   (ibuffer-mode . ajv/ibuffer-use-default-filter)
   (ibuffer-mode . ibuffer-auto-mode)
   )
  )

(use-package company
  ;; :bind (("S-<tab>" . company-complete))
  :config (global-company-mode)
  (setq company-idle-delay 0.1
	company-minimum-prefix-length 2
	company-selection-wrap-around t))

(use-package company-auctex
  :config (company-auctex-init))


(use-package expand-region
  :bind ("C-=" . er/expand-region))


(use-package ialign :demand)


(use-package smartparens-config
  :ensure smartparens
  :config (show-smartparens-global-mode)
  :hook
  (((prog-mode markdown-mode) . smartparens-mode)
   ((prog-mode markdown-mode) . show-smartparens-mode))
  )


(use-package god-mode
  :demand
  :bind (("<escape>" . god-mode-all)
	 (:map god-local-mode-map
	       ("." . repeat)
	       ("i" . god-mode-all)
	       ("<escape>" . (lambda () (interactive) (god-mode-activate)))))
  :hook ((god-mode-enabled . ajv/god-update-cursor)
  	 (god-mode-disabled . ajv/god-update-cursor))
  :config
  (use-package ajv-god
    :demand
    :bind ((:map god-local-mode-map
    		 ("q" . ajv/insert-string-from-god-mode)))
    :config (setq god-exempt-major-modes
		  (append ajv/god-exempt-modes god-exempt-major-modes)))
  (god-mode-all)
  )

(use-package god-mode-isearch
  :after (god-mode)
  :bind ((:map isearch-mode-map ("<escape>" . god-mode-isearch-activate))
	 (:map god-mode-isearch-map ("<escape>" . god-mode-isearch-disable)))
  )

(use-package which-key
  :config
  (which-key-mode)
  (which-key-enable-god-mode-support)
  )

(use-package aggressive-indent
  :config
  (add-to-list 'aggressive-indent-excluded-modes 'Messages)
  (global-aggressive-indent-mode 1)
  )

(use-package keyfreq
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

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


(use-package pdf-tools :defer 2 :magic ("%PDF" . pdf-view-mode) :pin manual
  :config
  (pdf-tools-install)
  (use-package ajv-pdf :demand
    :bind (:map pdf-view-mode-map
		;; ("q" . delete-frame)
		("q" . image-kill-buffer)
		("M-m" . ajv/pdf-view-toggle-modeline)
		("M-i" . pdf-view-midnight-minor-mode))
    :config
    (setq pdf-view-resize-factor 1.05
	  auto-revert-interval 0.1
	  auto-revert-verbose nil)
    (advice-add #'ido-find-file :filter-return #'ajv/pdf-launch-file)
    (advice-add #'find-file :filter-return #'ajv/pdf-launch-file)
    :hook ((pdf-view-mode . ajv/pdf-view-save-disable-modeline-format)
	   (pdf-view-mode . ajv/pdf-view-disable-linum-mode)
	   (pdf-view-mode . auto-revert-mode)
	   (pdf-view-mode . pdf-misc-size-indication-minor-mode)))
  )

(use-package pdf-view-restore
  :after pdf-tools
  :config (setq pdf-view-restore-filename "~/.emacs.d/.pdf-view-restore")
  :hook (pdf-view-mode . pdf-view-restore-mode)
  )


(use-package magit
  :bind (("<f2>" . magit-status))
  :config
  (use-package ajv-magit :demand
    :bind (:map magit-status-mode-map
		([remap magit-mode-bury-buffer] . ajv/magit-kill-buffers)))
  (setq magit-completing-read-function 'magit-ido-completing-read
	magit-diff-refine-hunk t)
  )

(use-package github-explorer :commands github-explorer)


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
   ("s-*" . ajv/switch-buffer-scratch-other-window)
   ("s-`" . ajv/open-home-in-dired)
   ("s-~" . ajv/open-symlink-folder-in-dired)
   ("s-p" . ajv/mypaths)
   ("s-P" . ajv/mypaths-other-window)
   ("C-c w c". ajv/window-config)
   ("<f8>". ajv/window-config)
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
   ("s-F" . ido-find-file-other-window)
   ("s-g" . keyboard-quit))
  :config
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
  (("s-a" . org-agenda)
   ("<f10>" . (lambda () (interactive) (switch-to-buffer "*Org Agenda*"))))
  :config
  (setq org-modules (quote
		     (org-bbdb org-bibtex org-docview org-gnus org-habit org-info org-irc org-mhe org-rmail org-w3m)))
  ;; (add-to-list 'org-modules 'org-habit)
  :hook ((after-init . org-agenda-list))
  )

(use-package ajv-elfeed
  :init (use-package elfeed
	  :hook ((elfeed-search-mode . toggle-truncate-lines)))
  :config
  (use-package elfeed-org
    ;; http://pragmaticemacs.com/emacs/read-your-rss-feeds-in-emacs-with-elfeed
    :config
    (elfeed-org)
    (setq rmh-elfeed-org-files ajv/my-elfeed-org-file)
    )
  :hook ((after-init . ajv/kill-elfeed-log-buffer))
  )

(use-package ajv-latex
  :demand t
  :mode ("\\.tex" . LaTeX-mode)
  :config
  (eval-after-load 'latex
    '(define-key LaTeX-mode-map (kbd "<f5>") 'TeX-command-master))
  :hook (LaTeX-mode . turn-on-reftex)
  )

(use-package markdown-toc :demand)

(use-package ajv-play-music
  :commands ajv/play-this-music
  :bind
  (("<XF86AudioPlay>" . ajv/play-pause-music)
   ("<XF86AudioPause>" . ajv/stop-music)
   ("<XF86AudioNext>" . ajv/play-next-music)
   ("<XF86AudioPrev>" . ajv/play-previous-music)
   ("<XF86Search>" . ajv/play-this-music))
  )


(use-package ajv-misc
  :defer 1
  :init (setq inhibit-startup-message t)
  :hook ((before-save . time-stamp)
	 (after-save . executable-make-buffer-file-executable-if-script-p))
  )

(use-package buffer-move
  :bind
  (("<f11>" . buf-move-left)
   ("<f12>" . buf-move-right)))

(use-package ajv-visual)

(use-package ajv-modeline)
