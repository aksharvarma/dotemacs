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
(setq use-package-verbose t)
(setq use-package-compute-statistics t)
;; Use full hook names so that help commands get contextual awareness
(setq use-package-hook-name-suffix nil)
(use-package diminish)
(use-package delight)
(use-package bind-key)

;; Start loading up other things
;; (use-package cl)

(use-package ajv-my-functions :demand
  :bind
  (("s-8" . ajv/switch-buffer-scratch)
   ("s-*" . ajv/switch-buffer-scratch-other-window)
   ("s-`" . ajv/open-home-in-dired)
   ("s-~" . ajv/open-symlink-folder-in-dired)
   ("s-p" . ajv/mypaths)
   ("s-P" . ajv/mypaths-other-window)
   ("C-c w c". ajv/create-my-window-config)
   ("<f8>". ajv/create-my-window-config)
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
   ("C-+" . ajv/increase-frame-font)
   (:map help-mode-map
	 ("q" . (lambda () (interactive) (ajv/kill-this-buffer) (other-window 1)))))
  :config
  (when window-system
    (global-set-key (kbd "C-x C-c") 'ajv/ask-before-closing))
  (advice-add 'revert-buffer :around #'yes-or-no-p->-y-or-n-p)
  (global-set-key [remap goto-line] 'ajv/goto-line-with-feedback)
  (setq ajv/settings/timer-to-periodically-show-window-config
	(run-with-idle-timer ;; (* 60 ajv/settings/period-for-showing-window-config)
	 ajv/settings/period-for-showing-window-config-in-seconds
	 t 'ajv/create-my-window-config-in-primary-frame))
  :hook
  ((find-file-hook . ajv/rename-symlink-buffer-with-truename)
   (emacs-startup-hook . ajv/measure-loading-time)
   (before-save-hook . ajv/delete-trailing-whitespace)
   ;; (emacs-startup-hook . ajv/create-my-window-config)
   )
  )

;; (remove-hook 'find-file-hook 'ajv/rename-symlink-buffer-with-truename)
(add-hook 'find-file-hook 'ajv/rename-symlink-buffer-with-truename)
(use-package notmuch
  :commands notmuch notmuch-jump-search notmuch-search ajv/notmuch/start-notmuch
  :config
  ;; Arch linux's notmuch installation comes with emacs stuff.
  ;; "Due to the dependency on the command line interface, the Notmuch Emacs interface version must be compatible with the Notmuch version. On Linux, the easiest way to ensure this is to use the package(s) in your distribution's package repository."
  ;; https://notmuchmail.org/notmuch-emacs/
  ;;
  ;; The following lines ensures that we use that notmuch code.
  (add-to-list 'load-path "/usr/share/emacs/site-lisp/")
  (use-package ajv-notmuch :demand
    :bind ((:map notmuch-show-mode-map
		 ("u" . ajv/notmuch/show-toggle-unread)
		 ("U" . ajv/notmuch/show-toggle-unread)
		 ("<" . w3m-previous-anchor)
		 (">" . w3m-next-anchor)
		 ("B" . ajv/notmuch/copy-link))
	   (:map notmuch-tree-mode-map
		 ("u" . ajv/notmuch/tree-toggle-unread)
		 ("U" . ajv/notmuch/tree-toggle-unread))
	   (:map notmuch-search-mode-map
		 ("u" . ajv/notmuch/search-toggle-unread)
		 ("U" . ajv/notmuch/search-toggle-unread)
		 ("g" . notmuch-poll-and-refresh-this-buffer))
	   (:map notmuch-hello-mode-map
		 ("g" . notmuch-poll-and-refresh-this-buffer)
		 ("k" . ajv/notmuch/clear-searches)
		 ("." . ajv/notmuch/set-initial-cursor-position)))
    :hook (notmuch-hello-refresh-hook . ajv/notmuch/set-initial-cursor-position))
  (setq ajv/notmuch/timer-to-periodically-update-notmuch
	(run-at-time t ajv/notmuch/period-for-updating-notmuch-in-seconds
		     'ajv/notmuch/poll-and-refresh-quietly))
  (ajv/notmuch/alert-enable-mode-line-display)
  )

(use-package message
  :after notmuch
  ;; :commands
  ;; message-forward-subject-fwd
  :config
  (setq message-citation-line-function
        'message-insert-formatted-citation-line)
  (setq message-citation-line-format
        "On %a, %b %d %Y at %r, %f wrote:")
  (setq message-make-forward-subject-function
        #'message-forward-subject-fwd)
  (setq message-auto-save-directory ajv/sensitive/message-auto-save-directory)
  (setq message-send-mail-partially-limit nil)
  (setq mail-specify-envelope-from t)
  (setq message-sendmail-envelope-from 'header)
  (setq mail-envelope-from 'header)
  ;; The following line is so as to fix the `i-did-not-set--mail-host-address--so-tickle-me'
  ;; text that shows up in buffers when notmuch visits emails
  (setq mail-host-address (system-name))
  (setq message-send-mail-function 'message-send-mail-with-sendmail)
  (setq message-forward-as-mime t)
  :hook
  (message-sent-hook . ajv/notmuch/flush-msmtpq)
  )


(use-package sendmail
  :after notmuch
  :config
  (setq sendmail-program "msmtpq")
  )

(use-package ido :demand
  :ensure t
  :bind (:map ido-common-completion-map
  	      ("<C-return>" . ido-magic-delete-char))
  :config
  (ido-mode t)
  (ido-everywhere)
  (setq ido-enable-flex-matching t)
  (setq ido-auto-merge-work-directories-length -1)
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

(use-package hl-line
  :config
  (setq hl-line-sticky-flag nil)
  (defun ajv/hl-line/set-face () (interactive)
	 (set-face-attribute 'hl-line nil
			     :foreground nil
			     :background (color-darken-name
					  (face-background 'default) 10)))
  (ajv/make-enable-disable-defuns hl-line-mode hl-line 1 -1))

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
	 ("H" . ajv/ibuffer/default-filter-folding)
	 ))
  :config
  (use-package ibuffer-vc :demand)
  (ajv/make-enable-disable-defuns ibuffer-auto-mode ibuffer-auto 1 -1)
  :hook
  (;; (ibuffer-mode . ajv/ibuffer/group-by-vc)
   (ibuffer-mode-hook . ajv/ibuffer/use-default-filter)
   (ibuffer-mode-hook . ibuffer-auto-mode)
   (ibuffer-mode-hook . ajv/hl-line/enable)
   (ibuffer-mode-hook . ajv/ibuffer-auto/enable)))

(use-package company
  ;; :bind (("S-<tab>" . company-complete))
  :config (global-company-mode)
  (defun ajv/company-idle-delay ()
    (if (eq major-mode
	    'inferior-python-mode)
	nil 0.1))
  (setq company-idle-delay 'ajv/company-idle-delay)
  (setq company-minimum-prefix-length 2)
  (setq company-selection-wrap-around t))

(use-package company-auctex
  :mode ("\\.tex\\'" . LaTeX-mode)
  :config (company-auctex-init))


(use-package flyspell :diminish ""
  :config (setq flyspell-issue-welcome-flag nil)
  (setq flyspell-issue-message-flag nil)
  (setq ispell-program-name "aspell")    ; use aspell instead of ispell
  :bind ("<mouse-3>" . flyspell-correct-word)
  :hook ((markdown-mode-hook text-mode-hook LaTeX-mode-hook org-mode-hook) . flyspell-mode))

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
  :hook (prog-mode-hook . highlight-numbers-mode))

(use-package rainbow-mode :diminish :commands rainbow-mode
  :hook ((prog-mode-hook . rainbow-mode)))

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
  :hook ((god-mode-enabled-hook . ajv/god/god-mode-has-priority)
	 (god-mode-enabled-hook . ajv/god/update-cursor)
	 (god-mode-disabled-hook . ajv/god/update-cursor))
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
  :delight which-key-mode "" "which-key"
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
                                     (lambda nil (ansi-term shell-pop-term-shell)))))
  (setq shell-pop-term-shell "/bin/bash")
  ;; need to do this manually or not picked up by `shell-pop'
  (shell-pop--set-shell-type 'shell-pop-shell-type shell-pop-shell-type))


(use-package indent-tools)

(use-package highlight-indentation
  :hook ((prog-mode-hook . highlight-indentation-mode)
	 (prog-mode-hook . highlight-indentation-current-column-mode)))

(use-package yaml-mode
  :mode ("\\.yaml\\'" . yaml-mode)
  :hook ((yaml-mode-hook . highlight-indentation-mode)
	 (yaml-mode-hook . highlight-indentation-current-column-mode))
  :bind ((:map yaml-mode-map
	       ("C-m" . newline-and-indent)
	       ("RET" . newline-and-indent)
	       ("C-c >" . indent-tools-hydra/body))))


(ajv/make-enable-disable-defuns company-mode company 1 -1)
(ajv/make-enable-disable-defuns yas/minor-mode yas/minor-mode 1 -1)
(use-package json-mode :diminish
  :hook ((json-mode-hook . ajv/company/disable)
	 (json-mode-hook . ajv/yas/minor-mode/disable)))

(use-package logview :demand)

(use-package python :defer 2
  :mode ("\\.py\\'" . python-mode)
  :commands python-mode
  :delight python-mode
  :bind
  ((:map elpy-mode-map
	 ("s-c" . elpy-shell-send-region-or-buffer)
	 ("s-C" . (lambda () (interactive)
		    (let ((current-prefix-arg '-)) ;; emulate C-u
		      (call-interactively 'elpy-shell-send-region-or-buffer))))
	 ("C-c <" . indent-tools-hydra/body)))
  :config
  (elpy-enable)
  (setq elpy-rpc-python-command "python")
  ;; (setq python-shell-interpreter "ipython")
  ;; (setq python-shell-interpreter-args "-i --simple-prompt")
  (setq elpy-autodoc-delay 0.1)
  (setq elpy-syntax-check-command "pyflakes")
  (setq elpy-rpc-large-buffer-size 65536)
  (setq elpy-rpc-backend "jedi")
  (setq elpy-rpc-virtualenv-path 'current)
  ;; (setq elpy-rpc-virtualenv-path 'system)
  (setq elpy-shell-starting-directory 'current-directory)
  ;; If you want to use the Jupyter console instead of IPython
  ;; Not being used at the moment.
  (setq python-shell-interpreter "jupyter")
  (setq python-shell-interpreter-args "console --simple-prompt")
  (setq python-shell-prompt-detect-failure-warning nil)
  (add-to-list 'python-shell-completion-native-disabled-interpreters "jupyter")

  ;; Adjust elpy-modules, delete flymake because we'll use flycheck
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-to-list 'elpy-modules 'elpy-module-folding) ;add folding help
  (add-to-list 'elpy-modules 'elpy-module-autodoc) ;auto update docs
  (add-to-list 'dired-guess-shell-alist-user '("\\.py\\'" "python"))
  :hook (elpy-mode-hook . (lambda () (elpy-shell-toggle-dedicated-shell 1))))

(use-package flycheck :demand :after python :diminish
  :hook ((elpy-mode-hook . flycheck-mode)))

(use-package py-autopep8
  :hook ((elpy-mode-hook . py-autopep8-enable-on-save)))

(use-package ein
  :bind ((:map ein:notebook-mode-map ("s-s" . ein:notebook-save-notebook-command-km)))
  :hook ((ein:markdown-mode-hook . ajv/company/disable)
	 (ein:notebook-mode-hook . ajv/company/disable)))

(use-package flymake
  ;; TODO: Change this to flycheck
  ;; https://www.masteringemacs.org/article/spotlight-flycheck-a-flymake-replacement
  :commands flymake-mode
  :init
  (setq flymake-suppress-zero-counters t)
  (setq flymake-start-on-flymake-mode t)
  (setq flymake-no-changes-timeout nil)
  (setq flymake-start-on-save-buffer t)
  (setq flymake-proc-compilation-prevents-syntax-check t)
  :config
  (defun ajv/flymake/run () (interactive)
	 (flymake-mode 1) (flymake-start) (flymake-show-diagnostics-buffer))
  (defun ajv/flymake/stop () (interactive)
	 (flymake-stop-all-syntax-checks)
	 (kill-buffer (flymake--diagnostics-buffer-name))
	 (flymake-mode 0))
  (defun ajv/flymake/next-line-and-show () (interactive)
	 (forward-button 1 nil nil nil) (flymake-show-diagnostic (point)))
  (defun ajv/flymake/previous-line-and-show () (interactive)
	 (forward-button -1 nil nil nil) (flymake-show-diagnostic (point)))
  :bind (:map flymake-diagnostics-buffer-mode-map
	      ("n" . ajv/flymake/next-line-and-show)
	      ("p" . ajv/flymake/previous-line-and-show))
  :hook (flymake-diagnostics-buffer-mode-hook . visual-line-mode))

(use-package flymake-proselint :after flymake
  :init
  (dolist (mode '("markdown-mode" "text-mode"))
    (add-hook (intern (concat mode "-hook")) #'flymake-proselint-setup)))

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
  (setq rg-command-line-flags '("--pcre2 --follow")))

(use-package anzu :demand :diminish
  :config
  (global-anzu-mode +1)
  (global-set-key [remap query-replace] 'anzu-query-replace)
  (global-set-key [remap query-replace-regexp] 'anzu-query-replace-regexp))

(use-package browse-kill-ring :demand
  :config
  (browse-kill-ring-default-keybindings)
  (setq browse-kill-ring-highlight-current-entry t)
  (setq browse-kill-ring-highlight-inserted-item t))

(use-package undo-tree :demand
  :diminish
  :config (global-undo-tree-mode))

;; (use-package smex
;;   :bind (("M-x" . smex))
;;   :config (smex-initialize))

(use-package amx :demand
  :commands amx-mode
  :config (amx-mode)
  :bind (("M-x" . amx)))

(use-package pdf-tools :defer 2 :magic ("%PDF" . pdf-view-mode) :pin manual
  :bind (:map pdf-view-mode-map
	      ("q" . image-kill-buffer)
	      ("h" . image-backward-hscroll)
	      ("j" . pdf-view-next-line-or-next-page)
	      ("k" . pdf-view-previous-line-or-previous-page)
	      ("l" . image-forward-hscroll)
	      ("u" . pdf-view-scroll-down-or-previous-page)
	      ("d" . pdf-view-scroll-up-or-next-page)
	      ("f" . image-forward-hscroll)
	      ("b" . image-backward-hscroll)
	      ("M-i" . pdf-view-midnight-minor-mode))
  :config
  (pdf-tools-install)
  (use-package ajv-pdf :demand
    :bind (:map pdf-view-mode-map
		("M-m" . ajv/pdf-tools/toggle-modeline)))
  (when (featurep 'ido)
    (advice-add #'ido-find-file :filter-return #'ajv/pdf-tools/launch-file))
  (setq pdf-view-resize-factor 1.05)
  (setq auto-revert-interval 0.1)
  (setq auto-revert-verbose nil)
  :hook ((pdf-view-mode-hook . ajv/pdf-tools/save-disable-modeline-format)
	 (pdf-view-mode-hook . ajv/pdf-tools/disable-linum-mode)
	 (pdf-view-mode-hook . auto-revert-mode)
	 (pdf-view-mode-hook . pdf-misc-size-indication-minor-mode))
  )

(use-package pdf-view-restore
  :after pdf-tools
  :config
  (setq pdf-view-restore-filename (concat user-emacs-directory ".pdf-view-restore"))
  (setq use-file-base-name-flag nil)
  ;; :config (setq pdf-view-restore-filename "~/.emacs.d/.pdf-view-restore")
  :hook (pdf-view-mode-hook . pdf-view-restore-mode)
  )


(use-package nov
  :demand
  :bind
  (:map nov-mode-map
	("j" . nov-scroll-up)
	("k" . nov-scroll-down)
	("f" . nov-history-forward)
	("b" . nov-history-back)
	("M-m" . ajv/nov/toggle-modeline))
  :config
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
  (setq nov-text-width 80)
  (defun ajv/nov/font-setup ()
    (face-remap-add-relative 'variable-pitch :family "Liberation Sans"
                             :height 1.25))

  (defvar ajv/nov/mode-modeline-format)
  (defun ajv/nov/save-disable-modeline-format ()
    "Removes mode-line when in nov-mode. Use toggle function to show/hide."
    (interactive)
    (setq ajv/nov/mode-modeline-format mode-line-format)
    (setq mode-line-format nil))

  (defun ajv/nov/toggle-modeline ()
    "Toggles displaying the mode-line when in nov-mode."
    (interactive)
    (if mode-line-format
	(setq mode-line-format nil)
      (setq mode-line-format ajv/nov/mode-modeline-format))
    (force-mode-line-update 1)		;Needed to actually see the change
    )

  (defun ajv/nov/margin-and-width-adjustments ()
    (let ((total (window-width))
	  (left-margin (floor (/ (window-width) 4)))
	  (text-width (floor (/ (window-width) 2))))
      (when (eq major-mode "EPUB")
	(setq nov-text-width text-width)
	(set-window-margins nil left-margin))))

  (defun ajv/nov/window-configuration-change-hook-fn ()
    (ajv/nov/post-html-render-margin-adjustments)
    (remove-hook 'window-configuration-change-hook
		 'ajv/nov/window-configuration-change-hook-fn
		 t))
  (defun ajv/nov/post-html-render-margin-adjustments ()
    (let ((total (window-width))
	  (left-margin (floor (/ (window-width) 4)))
	  (text-width (floor (/ (window-width) 2))))
      (setq nov-text-width text-width)
      (set-window-margins nil left-margin))
    (add-hook 'window-configuration-change-hook
              'ajv/nov/window-configuration-change-hook-fn
              nil t))

  (defun ajv/nov/dedicated-frame-on-mode-start ()
    (interactive)
    (let* ((buffername (buffer-name))
	   (orig-margins (window-margins))
	   (old-left-margins (if orig-margins
				 (car orig-margins)
			       0))
	   (window-to-redisplay (selected-window)))
      (bury-buffer)
      (switch-to-buffer-other-frame (buffer-name))
      ;; (make-frame-command)
      (other-frame -1)
      ;; (bury-buffer)
      ;; (other-frame 1)
      ;; (switch-to-buffer-other-frame (buffer-name))
      ;; (other-frame -1)
      (set-window-margins nil old-left-margins)
      (force-window-update window-to-redisplay)
      (redisplay)
      (other-frame 1)
      )
    ;; (let ((orig-frame (selected-frame)))
    ;;   (make-frame-command)
    ;;   (switch-to-buffer-other-frame (buffer-name)))
    )

  ;; (lambda () (make-frame-command) (other-frame -1) (bury-buffer))

  :hook (;; (change-major-mode-after-body-hook . ajv/nov/dedicated-frame-on-mode-start)
	 (nov-mode-hook . ajv/nov/dedicated-frame-on-mode-start)
	 (nov-mode-hook . ajv/nov/font-setup)
	 (nov-mode-hook . ajv/nov/save-disable-modeline-format)
	 (nov-post-html-render-hook . ajv/nov/post-html-render-margin-adjustments))
  )

(remove-hook 'nov-mode-hook 'ajv/nov/dedicated-frame-on-mode-start)
(remove-hook 'nov-post-html-render-hook 'ajv/nov/post-html-render-margin-adjustments)

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
  (setq magit-completing-read-function 'magit-ido-completing-read)
  (setq magit-diff-refine-hunk t)
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
  (use-package dired-narrow
    :bind (:map dired-mode-map
		("/" . dired-narrow-regexp)))
  (use-package ajv-dired :demand
    :bind  (:map dired-mode-map
		 ("s". ajv/dired/sort-criteria)
		 ("l" . ajv/dired/launch-file)
		 ("C-c C-d C-b" . ajv/dired/delete-backup-files)
		 ("W" . ajv/dired/copy-directory-name-as-kill)
		 ("e" . dired-toggle-read-only) ;short binding for C-x C-q
		 ("q" . dired-toggle-read-only) ;consistency to god-mode binding
		 (")" . ajv/dired/toggle-symlink-dereferencing)
		 ;; ("M-<" . ajv/dired/go-to-beginning-of-buffer)
		 ;; ("M->" . ajv/dired/go-to-end-of-buffer)
		 )
    :hook ((dired-mode-hook . ajv/dired/set-default-sorting)
	   (dired-mode-hook . ajv/dired/hide-details-omit-hidden-files)
	   (dired-mode-hook . ajv/hl-line/enable)))
  (use-package dired-async
    :config
    (dired-async-mode 1))
  (use-package wdired
    :config
    (setq wdired-use-dired-vertical-movement 'sometimes)
    (advice-add 'wdired-finish-edit :after #'ajv/hl-line/enable)
    :hook (wdired-mode-hook . ajv/hl-line/disable))

  (setq dired-dwim-target t)                     ;default copy to other window
  (setq dired-listing-switches ajv/dired/listing-switches-without-symlink)
  (setq dired-recursive-copies 'always)
  (add-to-list 'dired-guess-shell-alist-user '("\\.pdf\\'" "evince"))
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

(use-package ajv-org :demand
  :bind
  (("s-a" . org-agenda)
   ("C-c C-g" . org-goto)		;because org-journal overrides this.
   ("C-c l" . org-store-link)
   ("C-c c" . org-capture)
   ("<f10>" . (lambda () (interactive) (delete-other-windows) (switch-to-buffer "*Org Agenda*"))))
  :config
  (setq org-modules '(ol-bbdb ol-bibtex ol-docview ol-gnus org-habit ol-info ol-irc
			      ol-mhe ol-rmail ol-w3m org-tempo))
  (setq org-default-notes-file (concat ajv/sensitive/my-org-agenda-files-dir "/refile.org"))

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . nil)
     (python . nil)
     (latex . t)))

  ;; (advice-add 'org-agenda-to-appt :before #'ajv/org/remove-all-appts)
  ;; :hook ((org-after-todo-state-change-hook . ajv/org/clock-in-if-state-ongoing))
  )

(use-package org-bullets :hook ((org-mode-hook . org-bullets-mode)))

(use-package org-noter
  :commands (org-noter)
  :config
  (setq org-noter-default-notes-file-names
	`(,(file-truename (concat ajv/settings/symlink-folder
				  "research/org-research-notes.org"))))
  (setq org-noter-always-create-frame nil)
  (setq org-noter-doc-split-fraction (cons 0.618 0.5))
  (setq org-noter-auto-save-last-location t)
  (setq org-noter-kill-frame-at-session-end nil)
  (setq org-noter-insert-note-no-questions t)
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
  (setq org-journal-file-type 'weekly)
  (setq org-journal-dir (file-truename
			 (concat ajv/settings/symlink-folder "journal/")))
  (setq org-journal-created-property-timestamp-format "%Y-%m-%d")
  (setq org-journal-date-format "%A, %d %B %Y")
  (setq org-journal-file-format "%Y-%m-%d.org")
  (setq org-journal-time-format "%H:%M ")
  (setq org-journal-hide-entries-p nil)
  (defun ajv/org-journal/insert-title ()
    "Read a string from the minibuffer and then insert it as title of journal entry."
    ;; (interactive "sString: ")
    ;; Insert the text read in from the minibuffer
    (org-set-startup-visibility)
    (insert (read-string "Title: "))
    (org-return)
    (org-cycle)
    (if god-global-mode (god-mode-all) nil))
  :hook
  ;; Do not open org journal mode files with visual line mode.
  ((org-journal-mode-hook . (lambda () (visual-line-mode -1)))
   ;; After inserting the time stamp: Add a title, move to next line, indent
   (org-journal-after-entry-create-hook . ajv/org-journal/insert-title)))

(use-package org-super-agenda :demand
  :config
  (org-super-agenda-mode)
  (setq org-super-agenda-groups
	'((:name "Habits"
		 :habit t
		 :order 100)
	  (:name "To Refile"
		 :auto-property "REFILE"
		 :order 0)
	  ;;
	  (:name "Done today"
                 :log closed)
          (:name "Clocked today"
		 :log clock)
	  ;;
	  (:name "Overdue"
		 :deadline past
		 :order 1)
	  (:name "Due today"
		 :and (:deadline today :not (:log closed) :not (:todo "DONE"))
		 :order 2)
	  ;;
	  (:name "Quick Tasks"
                 :and (:effort< "0:05" :date today :not (:todo "DONE"))
		 :order 6)
	  (:name "Medium length Tasks"
                 :and (:effort< "0:31" :date today :not (:todo "DONE"))
		 :order 7)
	  (:name "Long Tasks"
                 :and (:effort> "0:30" :date today :not (:todo "DONE"))
		 :order 8)
	  ;;
	  (:name "Scheduled today"
		 :and (:scheduled today)
		 :order 4)
	  (:name "Leftover"
		 :and (:scheduled past :not (:log closed))
		 :order 3)
	  ;;
	  (:discard (:time-grid t))
	  (:todo "WAITING" :order 99)
	  (:name "Upcoming Deadlines"
		 :deadline future
		 :scheduled future
		 :order 80)
	  (:anything t
		     :order 99)))
  :bind (:map org-super-agenda-header-map
	      ("C-<tab>" . org-agenda-forward-block)
	      ("C-<S-iso-lefttab>" . org-agenda-backward-block)))

(use-package origami :after org-super-agenda
  :bind (:map org-super-agenda-header-map
	      ("C-." . origami-toggle-node))
  :config
  (defvar ajv/org-super-agenda/auto-hide-groups
    '("Upcoming Deadlines"))

  (defun ajv/org-super-agenda/origami-fold-default ()
    "Fold certain groups by default in Org Super Agenda buffer."
    (interactive)
    ;; (forward-line 3)
    ;; (cl-loop do (origami-forward-toggle-node (current-buffer) (point))
    ;;          while (origami-forward-fold-same-level (current-buffer) (point)))
    (--each ajv/org-super-agenda/auto-hide-groups
      (goto-char (point-min))
      (when (re-search-forward (rx-to-string `(seq " " ,it)) nil t)
        (origami-toggle-node (current-buffer) (point)))))
  :hook ((org-agenda-mode-hook . origami-mode)
	 (org-agenda-finalize-hook . ajv/org-super-agenda/origami-fold-default)))

(use-package ajv-org-roam :demand
  :straight nil
  :init
  (use-package org-roam :diminish)
  (setq-default org-roam-directory ajv/sensitive/my-org-roam-directory)
  ;; :custom (setq-defaultorg-roam-completion-everywhere t)
  :bind (("C-c k b" . org-roam-buffer-toggle)
	 ("C-c k f" . org-roam-node-find)
	 ("C-c k l" . org-roam-node-insert)
	 ("C-c k c" . org-roam-capture)
	 ("C-c k i" . org-id-get-create)
	 ("C-c k a" . org-roam-alias-add)
	 ("C-c k t" . org-roam-tag-add)
	 ("C-c k x" . ajv/org-roam/close-everything)
	 :map org-mode-map
	 ("C-M-i" . completion-at-point))
  :config
  (org-roam-setup)
  (org-roam-db-autosync-mode)
  (setq org-roam-node-display-template
	(concat "${type:15} ${title:*} " (propertize "${tags:10}" 'face 'org-tag))))

(use-package org-roam-ui :diminish :delight ""
  :config
  (defun ajv/org-roam/ui-start ()
    (interactive)
    (org-roam-ui-mode)
    (org-roam-ui-follow-mode)
    (message "http://localhost:35901/")
    (kill-new "http://localhost:35901/"))
  (defun ajv/org-roam/ui-stop ()
    (interactive)
    (org-roam-ui-follow-mode 0)
    (org-roam-ui-mode 0))
  (setq org-roam-ui-open-on-start nil)
  (diminish 'org-roam-ui-follow-mode)
  (ajv/org-roam/ui-start))

(use-package deft :diminish :delight
  :bind (("C-c k d" . deft)
	 (:map deft-mode-map
	       ("C-n" . forward-button)
	       ("C-p" . backward-button)))
  :commands (deft)
  :config
  (setq deft-directory "~/0/orkmasy")
  (setq deft-extensions '("org"))
  (setq deft-recursive t)
  ;; (setq deft-strip-title-regexp "\\(?:^%+\\|^#\\+TITLE: *\\|^[#* ]+\\|-\\*-[[:alpha:]]+-\\*-\\|^Title:[	 ]*\\|#+$\\)")

  ;; I want title picked from non-first line.
  ;; So I essentially disable deft's automatic calling (identity function)
  (setq deft-parse-title-function 'concat)
  ;; I adjust the title strip function to my needs
  (setq deft-strip-title-regexp ":PROPERTIES:\n\\(.+\n\\)+:END:\n")
  ;; This requires that it doesn't try to show me the template files
  ;; which have a different format.
  (setq deft-ignore-file-regexp "templates/.*")
  ;; Then I rewrite their function to do what I want.
  (defun deft-parse-title (file contents)
    (if deft-use-filename-as-title
	(deft-base-filename file)
      ;; (substring (nth 3 (split-string contents "\n" nil nil)) 8 nil)
      (let ((string
	     (nth 0
		  (split-string
		   (deft-chomp
		     (replace-regexp-in-string
		      deft-strip-title-regexp
		      "" contents))
		   "\n"))))
	(substring string 8 nil))))
  ;; (setq deft-org-mode-title-prefix t)
  ;; Finally, I want to ensure that any line that doesn't start with # or - is ignored.
  (setq deft-strip-summary-regexp "\\([-#:].*\n\\)+")
  ;; This also requires overwriting their function
  (defun deft-parse-summary (contents title)
    (let ((case-fold-search nil))
      (replace-regexp-in-string deft-strip-summary-regexp " " contents)))
  )

(use-package org-tree-slide :custom (org-image-actual-width nil))

(use-package org-present)

(use-package ajv-elfeed
  :straight nil
  :if (not (string-empty-p ajv/sensitive/my-elfeed-org-file))
  :init (use-package elfeed
	  :hook ((elfeed-search-mode-hook . toggle-truncate-lines)))
  :config
  (use-package elfeed-org
    ;; http://pragmaticemacs.com/emacs/read-your-rss-feeds-in-emacs-with-elfeed
    :config
    (elfeed-org)
    (setq rmh-elfeed-org-files ajv/my-elfeed-org-file-list))
  :hook ((after-init-hook . ajv/elfeed/kill-log-buffer)))


(use-package org-drill)
(use-package org-drill-table)

;; (use-package org-modern :config (global-org-modern-mode))

(use-package latex ;; :ensure auctex
  :straight auctex
  :mode ("\\.tex\\'" . LaTeX-mode)
  :config
  ;; The default enging to use to compile
  (setq-default TeX-engine 'xetex)
  ;; Various other default settings
  (setq LaTeX-command "latex -shell-escape --synctex=1 -interaction=batchmode")
  (setq LaTeX-command-style '(("" "%(PDF)%(latex) -shell-escape %(file-line-error) %(extraopts) %S%(PDFout)")))
  (setq TeX-save-query nil)                ;Don't ask before saving .tex files
  ;; To make AUCTeX read/update on changes to .bib files.
  (setq TeX-parse-self t) ; Enable parse on load. [DISABLED]
  (setq TeX-auto-save t) ; Enable parse on save. [DISABLED]
  ;; TeX-force-default-mode t
  (setq reftex-plug-into-AUCTeX t)
  (setq reftex-ref-macro-prompt nil)

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
  :hook ((LaTeX-mode-hook . turn-on-reftex)
	 (LaTeX-mode-hook . TeX-source-correlate-mode))
  )

(use-package markdown-toc :demand)

(use-package writeroom-mode :demand
  :config
  (setq writeroom-global-effects (writeroom-set-bottom-divider-width))
  (setq writeroom-maximize-window nil)
  (setq writeroom-local-effects '(visual-line-mode)))

(use-package ajv-play-music
  :straight nil
  :commands ajv/music/play-this
  :bind (("<XF86AudioPlay>" . ajv/music/play-pause)
	 ("<XF86AudioPause>" . ajv/music/stop-playing)
	 ("<XF86AudioNext>" . ajv/music/play-next)
	 ("<XF86AudioPrev>" . ajv/music/play-previous)
	 ("<XF86Search>" . ajv/music/play-this)))

;; emms
(use-package emms :diminish :delight
  :config
  (emms-all)
  (setq emms-player-list '(emms-player-mpv))
  (setq emms-info-functions '(emms-info-tinytag))
  (setq emms-source-file-default-directory "~/0/music/")
  ;; (setq emms-source-playlist-default-format nil)
  (setq emms-playlist-default-major-mode 'emms-playlist-mode)
  (defun ajv/emms/show-format-function (track)
    (if (emms-track-type 'file)
	((file-name-base track))
      ("Playing: non-file")))
  (setq emms-track-description-function 'ajv/emms/show-format-function)
  (setq emms-show-format "%s")
  (setq emms-player-mpv-parameters '("--quiet" "--realy-quiet" "--force-window=no" "--no-audio-display"))
  (setq emms-tag-editor-tagfile-functions
	'(("mp3" "eyeD3"
	   ((info-artist . "--artist")
	    (info-title . "--title")
	    (info-album . "--album")
	    (info-tracknumber . "--track")
	    (info-year . "--release-year")
	    (info-genre . "--genre")
	    (info-note . "--comment")
	    (info-albumartist . "--album-artist")
	    (info-composer . "--comment")
	    (info-performer . "--comment")
	    (info-date . "--comment")))
	  ("ogg" . emms-tag-editor-tag-ogg)
	  ("flac" . emms-tag-editor-tag-flac)
	  ("opus" . emms-tag-tracktag-file)))
  )

(use-package proced
  :commands proced
  :config
  (setq proced-auto-update-flag t)
  (setq proced-auto-update-interval 1)
  (setq proced-descend t)
  (setq proced-filter 'user)
  :bind (:map proced-mode-map
	      ("q" . ajv/kill-this-buffer)))

(use-package proced-narrow :diminish
  :after proced
  :bind (:map proced-mode-map
              ("/" . proced-narrow)))


(use-package beginend :demand :diminish beginend-global-mode ""
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
  :hook ((before-save-hook . time-stamp)
	 (package-menu-mode-hook . ajv/hl-line/enable)
	 ;; (text-mode-hook . (lambda () (setq comment-start "# ")))
	 ;; (remove-hook 'text-mode-hook '(lambda () (setq comment-start "# ")))
	 (after-save-hook . executable-make-buffer-file-executable-if-script-p)
	 ;; Enable hideshow-minor-mode
	 (prog-mode-hook . hs-minor-mode)
	 ;; move through camelCasedWords
	 (prog-mode-hook . subword-mode))
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

  (setq-default fill-column most-positive-fixnum)
  (setq-default visual-line-fringe-indicators '(nil right-curly-arrow))
  ;; The hostname part in `frame-title-format' taken from:
  ;; https://blog.lambda.cx/posts/eamacs-improved-frame-title/
  (setq-default frame-title-format '("%b @" (:eval (or (file-remote-p default-directory 'host) system-name)) " - Emacs"))
  (column-number-mode t)
  (set-fringe-style '(0 . nil))
  (add-to-list 'default-frame-alist '(fullscreen . fullboth)) ;maximize all frames
  (add-to-list 'default-frame-alist `(font . ,ajv/settings/prefered-font-name))
  :hook ((text-mode-hook . turn-on-auto-fill))
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

(use-package 2048-game :commands 2048-game
  :config
  (setq *2048-default-victory-value* 32768)
  (advice-add '2048-game
	      :before #'(lambda ()
			  (make-frame '((name . "2048")))
			  (other-frame 1)))
  (advice-add '2048-game
	      :after #'(lambda ()
			 (setq mode-line-format nil)
			 (text-scale-increase 5.5)
			 (set-window-margins nil (/ (- (window-total-width) 30) 3))))
  :bind (:map 2048-mode-map
	      ("q" . (lambda () (interactive) (ajv/kill-this-buffer) (delete-frame))))
  )

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
