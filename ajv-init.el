;; Control transfers from .emacs.d/init.el to here.

;; https://www.emacswiki.org/emacs/ProfileDotEmacs
;; Go to the `profile-dotemacs.el` file and modify the file name to what you want. Then run the following to profile it nicely.
;; Note: Only top level sexps are profiled, so you might want to make sure that what you really want to look at is actually at the top of the list.
;; emacs -Q -l ~/.emacs.d/site-lisp/profile-dotemacs.el -f profile-dotemacs

;; Increase garbage collection threshold during startup and brings it back to reasonable values at the end.
(setq gc-cons-threshold (* 500 1024 1024) ;500MB
      gc-cons-percentage 0.8)
(add-hook 'emacs-startup-hook (lambda () (setq gc-cons-threshold (* 100 1024 1024)
                                               gc-cons-percentage 0.1)))

;; Load basic settings directly.
(setq ajv/my-settings-file-name (concat user-emacs-directory "site-lisp/ajv/ajv-settings.el"))
(load ajv/my-settings-file-name)
;; Load the custom-file which only has package-selected-packages
;; This will later be used to ensure that all of those have been installed.
(setq custom-file (concat ajv/my-init-directory ajv/custom-file-name))
(load custom-file)

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
                         ("melpa" . "http://melpa.org/packages/"))
      package-menu-hide-low-priority t
      package-archive-priorities
      '(("MELPA Stable" . 10)
        ("GNU ELPA"     . 5)
        ("MELPA"        . 0)))
(package-initialize)

;; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

;; install the missing packages
(dolist (package package-selected-packages)
  (unless (package-installed-p package)
    (package-install package)))


;; Get use-package and its dependencies
(require 'use-package)
(setq use-package-verbose t
      use-package-compute-statistics t)
(use-package diminish)
(use-package delight)
(use-package bind-key)

;; Start loading up other things
;; (use-package cl)

(use-package notmuch
  :commands notmuch notmuch-jump-search notmuch-search
  :config
  (use-package ajv-notmuch :demand
    :bind ((:map notmuch-show-mode-map
		 ("u" . ajv/notmuch/show-toggle-unread)
		 ("U" . ajv/notmuch/show-toggle-unread))
	   (:map notmuch-tree-mode-map
		 ("u" . ajv/notmuch/tree-toggle-unread)
		 ("U" . ajv/notmuch/tree-toggle-unread))
	   (:map notmuch-search-mode-map
		 ("u" . ajv/notmuch/search-toggle-unread)
		 ("U" . ajv/notmuch/show-toggle-unread)
		 ("g" . notmuch-poll-and-refresh-this-buffer))
	   (:map notmuch-hello-mode-map
		 ("g" . notmuch-poll-and-refresh-this-buffer)
		 ("k" . ajv/notmuch/clear-searches)))
    :hook (notmuch-hello-refresh . ajv/notmuch/set-initial-cursor-position))
  )

(use-package ido :demand
  :ensure t
  :bind (:map ido-common-completion-map
  	      ("<C-return>" . ido-magic-delete-char))
  :config
  (ido-mode t)
  (ido-everywhere)
  (setq ido-enable-flex-matching t
	ido-auto-merge-work-directories-length -1)
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
  (;; (ibuffer-mode . ajv/ibuffer/group-by-vc)
   (ibuffer-mode . ajv/ibuffer/use-default-filter)
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
  :mode ("\\.tex\\'" . LaTeX-mode)
  :config (company-auctex-init))


(use-package flyspell :diminish ""
  :config (setq flyspell-issue-welcome-flag nil
		flyspell-issue-message-flag nil
		ispell-program-name "aspell")    ; use aspell instead of ispell
  :hook ((markdown-mode text-mode LaTeX-mode org-mode) . flyspell-mode))

(use-package expand-region
  :bind ("C-=" . er/expand-region))


(use-package ialign :demand)


(use-package smartparens-config
  :ensure smartparens
  :delight smartparens-mode " ()" "smartparens"
  :demand
  :config
  (show-smartparens-global-mode)
  (smartparens-global-mode)
  (sp-local-pair '(emacs-lisp-mode lisp-interaction-mode) "'" "'" :actions nil)
  :bind (("M-[" . sp-backward-unwrap-sexp)
	 ("M-]" . sp-unwrap-sexp))
  )

(use-package volatile-highlights
  :diminish
  :config (volatile-highlights-mode t))

(use-package god-mode
  :init
  (setq god-mode-enable-function-key-translation nil)
  :demand
  :bind (("<escape>" . god-mode-all)
	 (:map god-local-mode-map
	       ("." . repeat)
	       ("i" . god-mode-all)
	       ("<escape>" . (lambda () (interactive) (god-mode-activate)))))
  :hook ((god-mode-enabled . ajv/god/god-mode-has-priority)
	 (god-mode-enabled . ajv/god/update-cursor)
	 (god-mode-disabled . ajv/god/update-cursor))
  :config
  (use-package ajv-god
    :demand
    :bind ((:map god-local-mode-map
		 ("q" . ajv/god/insert-string-from-god-mode)))
    :config (setq god-exempt-major-modes
		  (append ajv/god/exempt-modes god-exempt-major-modes))))

(use-package god-mode-isearch
  :after (god-mode)
  :bind ((:map isearch-mode-map ("<escape>" . god-mode-isearch-activate))
	 (:map god-mode-isearch-map ("<escape>" . god-mode-isearch-disable)))
  )

(use-package which-key
  :diminish
  :config
  (which-key-mode)
  (which-key-enable-god-mode-support)
  )

(use-package aggressive-indent
  :diminish
  :config
  (add-to-list 'aggressive-indent-excluded-modes 'Messages)
  (global-aggressive-indent-mode 1)
  )

(use-package keyfreq
  :config
  (setq keyfreq-file (concat user-emacs-directory ".emacs.keyfreq"))
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
  :diminish "Py"
  :config
  (elpy-enable)
  (setq python-shell-interpreter "ipython"
        python-shell-interpreter-args "--TerminalInteractiveShell.simple_prompt=True"
	elpy-syntax-check-command "pyflakes"
	elpy-rpc-backend "jedi"
	elpy-rpc-virtualenv-path 'current)
  (use-package flycheck :demand
    :config
    (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
    (add-hook 'elpy-mode-hook 'flycheck-mode))
  ;; :bind (:map elpy-mode-map
  ;; 	      ("<f5>" . elpy-shell-region-or-buffer))
  )

(use-package move-text :config (move-text-default-bindings))

(use-package iedit)

(use-package rg
  :bind
  (:map rg-mode-map
	("C-c C-s" . wgrep-save-all-buffers))
  :config
  (rg-enable-menu)
  (setq rg-command-line-flags '("--pcre2")))

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
		("M-m" . ajv/pdf-tools/toggle-modeline)
		("M-i" . pdf-view-midnight-minor-mode))
    :config
    (setq pdf-view-resize-factor 1.05
	  auto-revert-interval 0.1
	  auto-revert-verbose nil)
    (when (featurep 'ido)
      (advice-add #'ido-find-file :filter-return #'ajv/pdf-tools/launch-file))
    :hook ((pdf-view-mode . ajv/pdf-tools/save-disable-modeline-format)
	   (pdf-view-mode . ajv/pdf-tools/disable-linum-mode)
	   (pdf-view-mode . auto-revert-mode)
	   (pdf-view-mode . pdf-misc-size-indication-minor-mode)))
  )

(use-package pdf-view-restore
  :after pdf-tools
  :config (setq pdf-view-restore-filename (concat user-emacs-directory ".pdf-view-restore")
		use-file-base-name-flag nil)
  ;; :config (setq pdf-view-restore-filename "~/.emacs.d/.pdf-view-restore")
  :hook (pdf-view-mode . pdf-view-restore-mode)
  )


(use-package magit
  :bind (("<f2>" . magit-status))
  :config
  (use-package ajv-magit :demand
    :bind (:map magit-status-mode-map
		([remap magit-mode-bury-buffer] . ajv/magit-kill-buffers))
    (:map ido-common-completion-map
	  ("<f2>" . ido-enter-magit-status)))
  (setq magit-completing-read-function 'magit-ido-completing-read
	magit-diff-refine-hunk t)
  )

(use-package ajv-git-gutter-fringe-settings
  :init (use-package git-gutter-fringe :delight git-gutter-mode)
  :config
  (global-git-gutter-mode t)
  (fringe-mode '(0 . nil))
  )

(use-package keychain-environment :demand :config (keychain-refresh-environment))

(use-package github-explorer :commands github-explorer)


(use-package dired :demand
  :delight dired-mode "Dired"
  :config
  (setq dired-dwim-target t                     ;default copy to other window
        dired-listing-switches "-a -l -L -h --group-directories-first --classify"
	dired-recursive-copies 'always
	wdired-use-dired-vertical-movement 'sometimes)
  (put 'dired-find-alternate-file 'disabled nil) ;allow 'a' in dired
  (load "dired-x")
  (use-package ajv-dired
    :bind  (:map dired-mode-map
		 ("s". ajv/dired-sort-criteria)
		 ("l" . ajv/dired-launch-file)
		 ("C-c C-d C-b" . ajv/delete-backup-files))
    :hook ((dired-mode . ajv/dired-set-default-sorting)
	   (dired-mode . ajv/dired-hide-details-omit-hidden-files)))
  )

(use-package dired-rainbow :config (use-package ajv-dired-rainbow))

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
   ("%" . ajv/match-paren)
   ("s-w" . ajv/kill-this-buffer)
   ("s-o" . ajv/kill-other-buffer)
   ("C-z" . bury-buffer)
   ("s-<tab>" . other-window)
   ("s-b" . ido-switch-buffer)
   ("s-B" . ido-switch-buffer-other-window)
   ("s-s" . save-buffer)
   ("s-f" . ido-find-file)
   ("s-F" . ido-find-file-other-window)
   ("s-g" . keyboard-quit)
   ("C-s" . isearch-forward-regexp)
   ("C-r" . isearch-backward-regexp)
   ("C-M-s" . isearch-forward)
   ("C-M-r" . isearch-backward)
   (:map help-mode-map
	 ("q" . (lambda () (interactive) (ajv/kill-this-buffer) (other-window 1)))))
  :config
  (when window-system
    (global-set-key (kbd "C-x C-c") 'ajv/ask-before-closing))
  (advice-add 'revert-buffer :around #'yes-or-no-p->-y-or-n-p)
  :hook
  ((find-file . ajv/rename-symlink-buffer-with-truename)
   (emacs-startup . ajv/measure-loading-time)
   (before-save . ajv/delete-trailing-whitespace)
   (emacs-startup . ajv/window-config))
  )

(use-package ajv-org
  :demand
  :bind
  (("s-a" . org-agenda)
   ("<f10>" . (lambda () (interactive) (switch-to-buffer "*Org Agenda*"))))
  :config
  (setq org-modules '(ol-bbdb ol-bibtex ol-docview ol-gnus org-habit ol-info ol-irc ol-mhe ol-rmail ol-w3m))
  )

(use-package org-bullets :hook ((org-mode . org-bullets-mode)))

(use-package ajv-elfeed
  :if (not (string-empty-p ajv/my-elfeed-org-file))
  :init (use-package elfeed
	  :hook ((elfeed-search-mode . toggle-truncate-lines)))
  :config
  (use-package elfeed-org
    ;; http://pragmaticemacs.com/emacs/read-your-rss-feeds-in-emacs-with-elfeed
    :config
    (elfeed-org)
    (setq rmh-elfeed-org-files ajv/my-elfeed-org-file-list)
    )
  :hook ((after-init . ajv/elfeed/kill-log-buffer))
  )


(use-package latex :ensure auctex
  :mode ("\\.tex\\'" . LaTeX-mode)
  :config
  ;; The default enging to use to compile
  (setq-default TeX-engine 'xetex)
  ;; Various other default settings
  (setq LaTeX-command "latex -shell-escape --synctex=1"
	LaTeX-command-style '(("" "%(PDF)%(latex) -shell-escape %(file-line-error) %(extraopts) %S%(PDFout)"))
	TeX-save-query nil                ;Don't ask before saving .tex files
	;; To make AUCTeX read/update on changes to .bib files.
	TeX-parse-self t ; Enable parse on load. [DISABLED]
	TeX-auto-save t ; Enable parse on save. [DISABLED]
	;; TeX-force-default-mode t
	reftex-plug-into-AUCTeX t
	reftex-ref-macro-prompt nil)

  ;; What program to select when viewing output.
  (setq TeX-view-program-selection '(((output-dvi has-no-display-manager) "dvi2tty")
				     ((output-dvi style-pstricks) "xdg-open")
				     (output-dvi "xdvi")
				     (output-pdf "PDF Tools")
				     (output-html "xdg-open")))

  ;; Load some helpful functions
  (use-package ajv-latex :demand t)

  :bind (:map LaTeX-mode-map
	      ("<f5>" . TeX-command-run-all)
	      ("<f6>" . TeX-next-error)
	      ;; Map dollar to self-insert-command to ensure that smartparens works.
	      ;; As suggested here: https://github.com/Fuco1/smartparens/issues/834
	      ("$" . self-insert-command))
  :hook ((LaTeX-mode . turn-on-reftex)
	 (LaTeX-mode . TeX-source-correlate-mode))
  )

(use-package markdown-toc :demand)

(use-package ajv-play-music
  :commands ajv/play-this-music
  :bind
  (("<XF86AudioPlay>" . ajv/music/play-pause)
   ("<XF86AudioPause>" . ajv/music/stop-playing)
   ("<XF86AudioNext>" . ajv/music/play-next)
   ("<XF86AudioPrev>" . ajv/music/play-previous)
   ("<XF86Search>" . ajv/music/play-this))
  )


(use-package ajv-misc
  :defer 1
  :init (setq inhibit-startup-screen t)
  :hook ((before-save . time-stamp)
	 (after-save . executable-make-buffer-file-executable-if-script-p)
	 (prog-mode . hs-minor-mode)	;Enable hideshow-minor-mode
	 (prog-mode . subword-mode))	;Allows moving through camelCasedWords
  )

(use-package buffer-move :bind (("<f11>" . buf-move-left)
				("<f12>" . buf-move-right)))

(use-package ajv-visual
  :demand
  :config
  (load-theme ajv/prefered-light-theme-name t t)
  (load-theme ajv/prefered-dark-theme-name)

  (setq-default fill-column most-positive-fixnum
		visual-line-fringe-indicators '(nil right-curly-arrow)
		frame-title-format '("%b [%m]"))
  (column-number-mode t)
  (scroll-bar-mode 0)
  (menu-bar-mode 0)
  (tool-bar-mode 0)
  (set-fringe-style '(0 . nil))
  (add-to-list 'default-frame-alist '(fullscreen . fullboth)) ;maximize all frames
  (add-to-list 'default-frame-alist `(font . ,ajv/prefered-font-name))
  :hook ((text-mode . turn-on-auto-fill))
  )

(use-package ajv-modeline)

(use-package memento-mori
  :config
  (setq memento-mori-birth-date ajv/my-birthdate)
  (memento-mori-mode))

;; To get org mode to latex for DnD5e LaTeX Template
(let ((default-directory (concat user-emacs-directory "site-lisp/emacs-org-dnd")))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))
(setq org-dnd-use-package t)
(require 'ox-dnd)

;; The article: You’re probably using the wrong dictionary
;; Link: http://jsomers.net/blog/dictionary
;; Getting this to work in Emacs via sdcv and sdcv-mode
;; http://mbork.pl/2017-01-14_I'm_now_using_the_right_dictionary
;; Source for some other dictionaries in the stardict format
;; http://download.huzheng.org/bigdict/
(use-package sdcv-mode)
(use-package-report)
;; To sort the use-package statistics buffer by loading time
(with-current-buffer "*use-package statistics*"
  (progn (tabulated-list-sort 3)
	 (tabulated-list-sort 3)
	 (beginning-of-buffer)))
