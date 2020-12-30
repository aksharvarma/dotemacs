;; Control transfers from .emacs.d/init.el to here.

;; https://www.emacswiki.org/emacs/ProfileDotEmacs
;; Go to the `profile-dotemacs.el` file and modify the file name to what you want. Then run the following to profile it nicely.
;; Note: Only top level sexps are profiled, so you might want to make sure that what you really want to look at is actually at the top of the list.
;; emacs -Q -l ~/.emacs.d/site-lisp/profile-dotemacs.el -f profile-dotemacs

;; Increase garbage collection, prevent some visual elements early on
;; Then setup package archives, initialize and install uninstalled packages
(load (concat user-emacs-directory "site-lisp/ajv/ajv-pre-setup.el"))

;; Everything after this point will be via use-package
;; Get use-package and its dependencies
(require 'use-package)
(setq use-package-verbose t
      use-package-compute-statistics t)
(use-package diminish)
(use-package delight)
(use-package bind-key)

;; Start loading up other things
;; (use-package cl)

;; TODO: change setq calls to be use-package :custom calls

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
		 ("U" . ajv/notmuch/search-toggle-unread)
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

(use-package ido-completing-read+
  :config
  (ido-ubiquitous-mode)
  ;; Increase ido-cr+-max-items to use ido in describe-(function|variable)
  (setq ido-cr+-max-items 40000))

(when (not (featurep 'ido))
  (fset 'ido-completing-read 'completing-read)
  (fset 'ido-find-file 'find-file)
  (fset 'ido-switch-buffer 'switch-to-buffer)
  (fset 'ido-switch-buffer-other-window 'switch-to-buffer-other-window))



(use-package yasnippet
  :init (setq yas-snippet-dirs '(ajv/settings/yasnippets-directory))
  :config
  (yas-global-mode 1)
  (yas-reload-all)
  (setq yas-prompt-functions
	'(yas-maybe-ido-prompt yas-completing-prompt yas-dropdown-prompt yas-no-prompt))
  :bind ((:map yas-minor-mode-map
	       ("C-c y" . #'yas-expand)))
  )

(use-package ajv-ibuffer :delight ibuffer-mode "IBuf" "ibuffer"
  :bind
  (("C-x C-b" . ibuffer)
   ("<f9>" . ibuffer)
   ("C-x C-S-b" . ibuffer-other-window)
   (:map ibuffer-mode-map
	 ("<up>" . ajv/ibuffer/previous-line)
	 ("<down>" . ajv/ibuffer/next-line)
	 ("s-f" . ajv/ibuffer/ido-find-file)
	 ("s-F" . ajv/ibuffer/ido-find-file-other-window)
	 ("M-<" . ajv/ibuffer/go-to-beginning-of-buffer)
	 ("M->" . ajv/ibuffer/go-to-end-of-buffer)
	 ("<" . ajv/ibuffer/go-to-beginning-of-buffer)
	 (">" . ajv/ibuffer/go-to-end-of-buffer)))
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
  :bind ("<mouse-3>" . flyspell-correct-word)
  :hook ((markdown-mode text-mode LaTeX-mode org-mode) . flyspell-mode))

(use-package expand-region
  :bind ("C-=" . er/expand-region))


(use-package ialign :demand)


(use-package smartparens-config
  :ensure smartparens
  :delight smartparens-mode "" "smartparens"
  :demand
  :config
  (show-smartparens-global-mode)
  (smartparens-global-mode)
  (sp-local-pair '(emacs-lisp-mode lisp-interaction-mode) "'" "'" :actions nil)
  :bind (("M-[" . sp-backward-unwrap-sexp)
	 ("M-]" . sp-unwrap-sexp))
  )

(use-package highlight-numbers :diminish
  :hook (prog-mode . highlight-numbers-mode))

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
  :delight python-mode
  :config
  (elpy-enable)
  (setq elpy-rpc-python-command "python3"
	python-shell-interpreter "ipython3"
        python-shell-interpreter-args "--TerminalInteractiveShell.simple_prompt=True"
	elpy-autodoc-delay 0.1
	elpy-syntax-check-command "pyflakes"
	elpy-rpc-backend "jedi"
	elpy-rpc-virtualenv-path 'current)

  ;; ;; If you want to use the Jupyter console instead of IPython
  ;; ;; Not being used at the moment.
  ;; (setq python-shell-interpreter "jupyter"
  ;;       python-shell-interpreter-args "console --simple-prompt"
  ;; 	python-shell-prompt-detect-failure-warning nil)
  ;; (add-to-list 'python-shell-completion-native-disabled-interpreters "jupyter")

  ;; Adjust elpy-modules, delete flymake because we'll use flycheck
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-to-list 'elpy-modules 'elpy-module-folding) ;add folding help
  (add-to-list 'elpy-modules 'elpy-module-autodoc) ;auto update docs
  )

(use-package flycheck :demand :after python :diminish
  :hook ((elpy-mode . flycheck-mode)))

(use-package py-autopep8
  :hook ((elpy-mode . py-autopep8-enable-on-save)))

(use-package move-text :config (move-text-default-bindings))

(use-package iedit)

(use-package hungry-delete :diminish
  :config
  (global-hungry-delete-mode)
  (setq hungry-delete-join-reluctantly t))

(use-package rg
  :commands (rg-menu)
  :bind
  (("C-c s" . rg-menu)
   :map rg-mode-map
   ("C-c C-s" . wgrep-save-all-buffers))
  :config
  (rg-enable-menu)
  (setq rg-command-line-flags '("--pcre2")))

(use-package anzu :demand :diminish
  :config
  (global-anzu-mode +1)
  (global-set-key [remap query-replace] 'anzu-query-replace)
  (global-set-key [remap query-replace-regexp] 'anzu-query-replace-regexp))

(use-package browse-kill-ring :demand
  :config
  (browse-kill-ring-default-keybindings)
  (setq browse-kill-ring-highlight-current-entry t
	browse-kill-ring-highlight-inserted-item t))

(use-package undo-tree :demand
  :diminish
  :config (global-undo-tree-mode))

(use-package smex
  :bind (("M-x" . smex))
  :config (smex-initialize))


(use-package pdf-tools :defer 2 :magic ("%PDF" . pdf-view-mode) :pin manual
  :bind (:map pdf-view-mode-map
	      ("q" . image-kill-buffer)
	      ("j" . pdf-view-next-line-or-next-page)
	      ("k" . pdf-view-previous-line-or-previous-page)
	      ("u" . pdf-view-scroll-down-or-previous-page)
	      ("d" . pdf-view-scroll-up-or-next-page)
	      ("M-i" . pdf-view-midnight-minor-mode))
  :config
  (pdf-tools-install)
  (use-package ajv-pdf :demand
    :bind (:map pdf-view-mode-map
		("M-m" . ajv/pdf-tools/toggle-modeline)))
  (when (featurep 'ido)
    (advice-add #'ido-find-file :filter-return #'ajv/pdf-tools/launch-file))
  (setq pdf-view-resize-factor 1.05
	auto-revert-interval 0.1
	auto-revert-verbose nil)
  :hook ((pdf-view-mode . ajv/pdf-tools/save-disable-modeline-format)
	 (pdf-view-mode . ajv/pdf-tools/disable-linum-mode)
	 (pdf-view-mode . auto-revert-mode)
	 (pdf-view-mode . pdf-misc-size-indication-minor-mode))
  )

(use-package pdf-view-restore
  :after pdf-tools
  :config (setq pdf-view-restore-filename (concat user-emacs-directory ".pdf-view-restore")
		use-file-base-name-flag nil)
  ;; :config (setq pdf-view-restore-filename "~/.emacs.d/.pdf-view-restore")
  :hook (pdf-view-mode . pdf-view-restore-mode)
  )



;; Mode for .gitignore, .git/info/exclude, and git/ignore files.
(use-package gitignore-mode)

;; Mode for .gitconfig, .git/config, git/config, and .gitmodules files.
(use-package gitconfig-mode)

;; Mode for .gitattributes, .git/info/attributes, and git/attributes files.
(use-package gitattributes-mode)

(use-package magit
  :bind (("<f2>" . magit-status)
	 (:map ido-common-completion-map
	       ("<f2>" . ido-enter-magit-status)))
  :config
  (use-package ajv-magit :demand
    :bind (:map magit-status-mode-map
		([remap magit-mode-bury-buffer] . ajv/magit/kill-buffers)))
  (setq magit-completing-read-function 'magit-ido-completing-read
	magit-diff-refine-hunk t)
  ;; TODO: At some point figure out if we can provide a better location
  (transient-append-suffix 'magit-status-jump '(0 0 -1)
    '("c " "Commits" magit-jump-to-unpushed-to-upstream))
  )

;; Use gitignore-templates-insert or gitignore-templates-new-file
(use-package gitignore-templates)

(use-package hl-todo :demand :config (global-hl-todo-mode))

(use-package magit-todos :demand :after magit
  ;; TODO: remove these unbindings once magit-todos fixes it in version 1.6
  :bind ((:map magit-todos-item-section-map
	       ("jT" . nil)
	       ("jl" . nil)
	       ("j" . nil))
	 (:map magit-todos-section-map
	       ("jT" . nil)
	       ("jl" . nil)
	       ("j" . nil)))
  :config
  (magit-todos-mode)
  (transient-append-suffix 'magit-status-jump '(0 0 -1)
    '("t " "Todos" magit-todos-jump-to-todos)))

(use-package ajv-git-gutter-fringe
  :init (use-package git-gutter-fringe :delight git-gutter-mode)
  :config
  (global-git-gutter-mode t)
  (fringe-mode '(0 . nil))
  )

(use-package keychain-environment
  :after magit
  :config (keychain-refresh-environment))

(use-package github-explorer :commands github-explorer)


(use-package dired :demand
  :delight dired-mode "Dired"
  :config
  (use-package dired-x :demand
    ;; Swap keybindings for dired-jump and dired-jump-other-window
    :bind (("C-x C-j" . dired-jump)
	   ("C-x M-j" . dired-jump-other-window)))
  (use-package ajv-dired :demand
    :bind  (:map dired-mode-map
		 ("s". ajv/dired/sort-criteria)
		 ("l" . ajv/dired/launch-file)
		 ("C-c C-d C-b" . ajv/dired/delete-backup-files)
		 ("W" . ajv/dired/copy-directory-name-as-kill)
		 ("e" . dired-toggle-read-only) ;short binding for C-x C-q
		 (")" . ajv/dired/toggle-symlink-dereferencing)
		 ("M-<" . ajv/dired/go-to-beginning-of-buffer)
		 ("M->" . ajv/dired/go-to-end-of-buffer))
    :hook ((dired-mode . ajv/dired/set-default-sorting)
	   (dired-mode . ajv/dired/hide-details-omit-hidden-files)))

  (setq dired-dwim-target t                     ;default copy to other window
        dired-listing-switches ajv/dired/listing-switches-without-symlink
	dired-recursive-copies 'always
	wdired-use-dired-vertical-movement 'sometimes)
  (put 'dired-find-alternate-file 'disabled nil) ;allow 'a' in dired
  )

(use-package dired-rainbow :config (use-package ajv-dired-rainbow))
(use-package dired-collapse :demand)

(use-package dired-subtree :demand :after dired
  :config
  (setq dired-subtree-use-backgrounds nil)
  :bind (:map dired-mode-map
              ("<tab>" . dired-subtree-toggle)
              ("<C-tab>" . dired-subtree-cycle)
              ("<S-iso-lefttab>" . dired-subtree-remove)
	      ("f" . dired-subtree-next-sibling)
	      ("b" . dired-subtree-previous-sibling)))

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
   ("s-W" . ajv/kill-other-buffer)
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
   ("C-M-j". ajv/join-to-next-line)
   ("M-j". ajv/join-to-previous-line)
   ("M-+" . count-words)
   ("M-u" . upcase-dwim)
   ("M-l" . downcase-dwim)
   ("M-c" . capitalize-dwim)
   (:map help-mode-map
	 ("q" . (lambda () (interactive) (ajv/kill-this-buffer) (other-window 1)))))
  :config
  (when window-system
    (global-set-key (kbd "C-x C-c") 'ajv/ask-before-closing))
  (advice-add 'revert-buffer :around #'yes-or-no-p->-y-or-n-p)
  (global-set-key [remap goto-line] 'ajv/goto-line-with-feedback)
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
   ("C-c C-g" . org-goto)		;because org-journal overrides this.
   ("<f10>" . (lambda () (interactive) (switch-to-buffer "*Org Agenda*"))))
  :config
  (setq org-modules '(ol-bbdb ol-bibtex ol-docview ol-gnus org-habit ol-info ol-irc ol-mhe ol-rmail ol-w3m org-tempo))
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . nil)
     (python . nil)
     (latex . t)))
  )

(use-package org-bullets :hook ((org-mode . org-bullets-mode)))

(use-package org-noter
  :commands (org-noter)
  :config
  (setq org-noter-default-notes-file-names
	`(,(file-truename (concat ajv/settings/symlink-folder
				  "research/org-research-notes.org")))
	org-noter-always-create-frame nil
	org-noter-doc-split-fraction (cons 0.618 0.5)
	org-noter-auto-save-last-location t
	org-noter-kill-frame-at-session-end nil
	org-noter-insert-note-no-questions t)
  :bind ((:map org-noter-doc-mode-map
	       ("M-p" . org-noter-sync-prev-note)
	       ("M-n" . org-noter-sync-next-note)
	       ("C-M-n" . org-noter-sync-next-page-or-chapter)
	       ("C-M-p" . org-noter-sync-prev-page-or-chapter)
	       ("M-." . org-noter-sync-current-note)
	       ("C-M-." . org-noter-sync-current-page-or-chapter))))

(use-package org-journal :demand
  :init
  ;; Change default prefix key; needs to be set before loading org-journal
  (setq org-journal-prefix-key "C-c j ")
  :bind (:map org-mode-map ("C-c C-j" . org-journal-new-entry))
  :config
  (setq org-journal-file-type 'weekly
	org-journal-dir (file-truename
			 (concat ajv/settings/symlink-folder "journal/"))
	org-journal-created-property-timestamp-format "%Y-%m-%d"
	org-journal-date-format "%A, %d %B %Y"
	org-journal-file-format "%Y-%m-%d.org"
	org-journal-time-format "%H:%M "
	org-journal-hide-entries-p nil)
  (defun ajv/org-journal/insert-title ()
    "Read a string from the minibuffer and then insert it as title of journal entry."
    ;; (interactive "sString: ")
    ;; Insert the text read in from the minibuffer
    (org-set-startup-visibility)
    (insert (read-string "Title: "))
    (org-return)
    (org-cycle))
  :hook
  ;; Do not open org journal mode files with visual line mode.
  ((org-journal-mode . (lambda () (visual-line-mode -1)))
   ;; After inserting the time stamp: Add a title, move to next line, indent
   (org-journal-after-entry-create . ajv/org-journal/insert-title)))

(use-package ajv-elfeed
  :if (not (string-empty-p ajv/sensitive/my-elfeed-org-file))
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
  ;; Don't show reftex highlight in modeline
  (delight 'reftex-mode nil "reftex")

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

(use-package writeroom-mode :demand
  :config
  (setq writeroom-global-effects (writeroom-set-bottom-divider-width))
  (setq writeroom-maximize-window nil)
  (setq writeroom-local-effects '(visual-line-mode)))

(use-package ajv-play-music
  :commands ajv/music/play-this
  :bind
  (("<XF86AudioPlay>" . ajv/music/play-pause)
   ("<XF86AudioPause>" . ajv/music/stop-playing)
   ("<XF86AudioNext>" . ajv/music/play-next)
   ("<XF86AudioPrev>" . ajv/music/play-previous)
   ("<XF86Search>" . ajv/music/play-this))
  )

(use-package proced
  :commands proced
  :config
  (setq proced-auto-update-flag t)
  (setq proced-auto-update-interval 1)
  (setq proced-descend t)
  (setq proced-filter 'user))

(use-package proced-narrow :diminish
  :after proced
  :bind (:map proced-mode-map
              ("/" . proced-narrow)))


(use-package beginend :demand :diminish beginend-global-mode
  :config
  (dolist (mode beginend-modes) (diminish (cdr mode)))
  (beginend-global-mode 1))

(use-package ajv-misc :demand
  :config
  ;; Misc diminish and delight settings
  (delight '((highlight-indentation-mode nil "highlight-indentation")
	     (emacs-lisp-mode ".el" :major)
	     (markdown-mode "md" :major)
	     (hs-minor-mode nil "hideshow")
	     (subword-mode nil "subword")
	     (auto-revert-mode nil "autorevert")
	     (auto-fill-function "" t)))
  :hook ((before-save . time-stamp)
	 (after-save . executable-make-buffer-file-executable-if-script-p)
	 ;; Enable hideshow-minor-mode
	 (prog-mode . hs-minor-mode)
	 ;; move through camelCasedWords
	 (prog-mode . subword-mode))
  )

(use-package buffer-move :bind (("<f11>" . buf-move-left)
				("<f12>" . buf-move-right)))

(use-package ajv-scpaste
  :commands (ajv/scpaste/paste-region-or-buffer))

(use-package ajv-theme
  :demand
  :config
  (load-theme ajv/settings/prefered-light-theme-name t t)
  (load-theme ajv/settings/prefered-dark-theme-name)

  (setq-default fill-column most-positive-fixnum
		visual-line-fringe-indicators '(nil right-curly-arrow)
		frame-title-format '("%b [%m]"))
  (column-number-mode t)
  (set-fringe-style '(0 . nil))
  (add-to-list 'default-frame-alist '(fullscreen . fullboth)) ;maximize all frames
  (add-to-list 'default-frame-alist `(font . ,ajv/settings/prefered-font-name))
  :hook ((text-mode . turn-on-auto-fill))
  )

(use-package fancy-battery
  :config
  (setq fancy-battery-show-percentage t)
  (set-face-attribute 'fancy-battery-discharging nil
		      :inherit '(bold)
		      :foreground "LightSkyBlue"
		      :background "gray10")
  (fancy-battery-mode))

(use-package ajv-modeline)



(use-package keycast :commands (keycast-mode))

(use-package memento-mori
  :config
  (setq memento-mori-birth-date ajv/sensitive/my-birthdate)
  (memento-mori-mode))

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
