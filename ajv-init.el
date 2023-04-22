;; Control transfers from .emacs.d/init.el to here.

;; https://www.emacswiki.org/emacs/ProfileDotEmacs
;; Go to the `profile-dotemacs.el` file and modify the file name to what you want. Then run the following to profile it nicely.
;; Note: Only top level sexps are profiled, so you might want to make sure that what you really want to look at is actually at the top of the list.
;; emacs -Q -l ~/.emacs.d/site-lisp/profile-dotemacs.el -f profile-dotemacs

;; Increase garbage collection, prevent some visual elements early on
(load (concat user-emacs-directory "site-lisp/ajv/ajv-pre-setup-common.el"))

;; Then setup package archives, initialize and install uninstalled packages
;; (load (concat user-emacs-directory "site-lisp/ajv/ajv-pre-setup-package.el"))

;; Use straight for helm
(load (concat user-emacs-directory "site-lisp/ajv/ajv-pre-setup-straight.el"))

;; The package manager stuff has been done in pre-setup.
;; Now it is just use-package macros throughout.

;; (setq use-package-verbose t)
(setq use-package-compute-statistics t)
;; Use full hook names so that help commands get contextual awareness
(setq use-package-hook-name-suffix nil)
(use-package diminish)
(straight-use-package 'delight)
(use-package delight)
(use-package bind-key)

;; Start loading up other things
;; (use-package cl)

(use-package ajv-my-functions :demand
  :straight nil
  :bind
  (("s-8" . ajv/switch-buffer-scratch)
   ("s-*" . ajv/switch-buffer-scratch-other-window)
   ("s-`" . ajv/open-home-in-dired)
   ("s-~" . ajv/open-symlink-folder-in-dired)
   ("s-p" . ajv/mypaths)
   ("s-P" . ajv/mypaths-other-window)
   ;; ("s-0" . ajv/mypaths)
   ;; ("s-)" . ajv/mypaths-other-window)
   ("C-c w c". ajv/create-my-window-config)
   ("C-c w w". ajv/show-weekly-plan-table)
   ("<f8>". ajv/create-my-window-config)
   ("%" . ajv/match-paren)
   ("s-w" . ajv/kill-this-buffer)
   ("s-W" . ajv/kill-other-buffer)
   ("s-o" . ajv/kill-other-buffer)
   ("C-c z" . bury-buffer)
   ("s-<tab>" . other-window)
   ("s-b" . switch-to-buffer)
   ("s-B" . switch-to-buffer-other-window)
   ("s-s" . save-buffer)
   ("s-f" . find-file)
   ("s-F" . find-file-other-window)
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
   ("<f5>" . compile)
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
  (ajv/setup-scratch-buffer)
  :hook
  ((find-file-hook . ajv/rename-symlink-buffer-with-truename)
   (emacs-startup-hook . ajv/measure-loading-time)
   ;; (before-save-hook . ajv/delete-trailing-whitespace)
   (org-mode-hook . (lambda () (add-hook 'after-save-hook 'ajv/auto-tangle-emacs-config)))))


(use-package notmuch
  :straight nil
  :commands notmuch notmuch-jump-search notmuch-search ajv/notmuch/start-notmuch
  :config
  ;; Arch linux's notmuch installation comes with emacs stuff.
  ;; "Due to the dependency on the command line interface, the Notmuch Emacs interface version must be compatible with the Notmuch version. On Linux, the easiest way to ensure this is to use the package(s) in your distribution's package repository."
  ;; https://notmuchmail.org/notmuch-emacs/
  ;;
  ;; The following lines ensures that we use that notmuch code.
  (add-to-list 'load-path "/usr/share/emacs/site-lisp/")
  (use-package ajv-notmuch :demand
    :straight nil
    :commands ajv/notmuch/start-notmuch
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
  (ajv/notmuch/alert-enable-mode-line-display))

(use-package message :after notmuch
  :straight (:type built-in)
  ;; :commands message-forward-subject-fwd
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
  :hook (message-sent-hook . ajv/notmuch/flush-msmtpq))


(use-package sendmail :after notmuch
  :config (setq sendmail-program "msmtpq"))

(use-package w3m)

;; (use-package ido :demand
;;   :ensure t
;;   :bind ((:map ido-common-completion-map
;;   	       ("<C-return>" . ido-magic-delete-char))
;;
;; Add remap bindings here to replace find-file, switch-to-buffer and so on into their ido versions. Or does that happen automatically?
;;
;; 	 (:map ido-completion-map
;; 	       (" " . self-insert-command))
;; 	 (:map minibuffer-local-completion-map
;; 	       (" " . self-insert-command)))
;;   :config
;;   (ido-mode t)
;;   (ido-everywhere)

;;   (setq ido-enable-flex-matching t)
;;   (setq ido-auto-merge-work-directories-length -1)

;;   ;; Fix space issue in ido
;;   (defalias 'ido-complete-space 'self-insert-command)

;;   (add-to-list 'ido-ignore-buffers "^.*\\.pdf$")

;;   )

;; (use-package ido-completing-read+
;;   :config
;;   (ido-ubiquitous-mode)
;;   ;; Increase ido-cr+-max-items to use ido in describe-(function|variable)
;;   (setq ido-cr+-max-items 40000))

;; (when (not (featurep 'ido))
;;   (fset 'ido-completing-read 'completing-read)
;;   (fset 'ido-find-file 'find-file)
;;   (fset 'ido-switch-buffer 'switch-to-buffer)
;;   (fset 'ido-switch-buffer-other-window 'switch-to-buffer-other-window))


(use-package helm-mode :diminish " ⎈" :demand
  :straight helm
  :config
  ;; (use-package helm-config :demand)
  ;; (require 'helm-config)
  ;; (setq helm-move-to-line-cycle-in-source nil)
  ;; (add-to-list 'helm-boring-file-regexp-list ".*~$")

  (setq helm-split-window-inside-p t)
  (setq completion-styles '(partial-completion basic))
  (helm-mode 1)
  :bind (("C-c h" . helm-command-prefix)
	 ([remap switch-to-buffer] . helm-buffers-list)
	 ("M-o" . helm-occur)
	 ([remap find-file] . helm-find-files)
	 ([remap occur] . helm-occur)
	 ([remap dabbrev-expand] . helm-dabbrev)
	 ([remap execute-extended-command] . helm-M-x)
	 ([remap apropos-command] . helm-apropos)))


(use-package yasnippet
  :init (setq yas-snippet-dirs '(ajv/settings/yasnippets-directory))
  :config
  (yas-global-mode 1)
  (yas-reload-all)
  (setq yas-wrap-around-region t)
  (setq yas-prompt-functions
	'(yas-maybe-ido-prompt yas-completing-prompt yas-dropdown-prompt yas-no-prompt))
  :bind ((:map yas-minor-mode-map
	       ("C-c y" . #'yas-expand))))

(use-package auto-yasnippet :diminish :delight
  :config (setq aya-case-fold t))



(use-package tiny :delight :diminish
  :bind ("C-M-;" . tiny-expand))



(use-package hl-line
  :config
  (setq hl-line-sticky-flag nil)
  (defun ajv/hl-line/set-face () (interactive)
	 (set-face-attribute 'hl-line nil
			     :foreground nil
			     :background (color-darken-name
					  (face-background 'default) 10)))
  (ajv/make-enable-disable-defuns hl-line-mode hl-line 1 -1))

(use-package ajv-ibuffer :demand
  :straight nil
  :init
  (use-package ibuffer :demand
	       :straight (:type built-in)
	       )
  (use-package ibuf-ext :demand
	       :straight (:type built-in)
	       )
  :delight ibuffer-mode "IBuf" "ibuffer"
  :bind
  (("C-x C-b" . ibuffer)
   ("<f9>" . ibuffer)
   ("C-x C-S-b" . ibuffer-other-window)
   (:map ibuffer-mode-map
	 ("<up>" . ajv/ibuffer/previous-line)
	 ("<down>" . ajv/ibuffer/next-line)
	 ;; ("s-f" . ajv/ibuffer/ido-find-file)
	 ;; ("s-F" . ajv/ibuffer/ido-find-file-other-window)
	 ("s-f" . ajv/ibuffer/find-file)
	 ("s-F" . ajv/ibuffer/find-file-other-window)
	 ("H" . ajv/ibuffer/default-filter-folding)
	 ))
  :config
  ;; (use-package ibuffer-vc :demand)
  (ajv/make-enable-disable-defuns ibuffer-auto-mode ibuffer-auto 1 -1)
  ;; Hide all helm buffers from showing up in this.
  (add-to-list 'ibuffer-never-show-predicates "^\\*helm.+\\*$")
  ;; Hide all straight buffers from showing up in this.
  (add-to-list 'ibuffer-never-show-predicates "^\\*straight.+\\*$")
  :hook
  (;; (ibuffer-mode . ajv/ibuffer/group-by-vc)
   (ibuffer-load-hook . ajv/ibuffer/default-filter-folding)
   (ibuffer-mode-hook . ajv/ibuffer/use-default-filter)
   ;; (ibuffer-auto-mode-hook . ajv/ibuffer/default-filter-folding)
   (ibuffer-mode-hook . ibuffer-auto-mode)
   (ibuffer-mode-hook . ajv/hl-line/enable)
   (ibuffer-mode-hook . ajv/ibuffer-auto/enable)))

(use-package company
  ;; :bind (("S-<tab>" . company-complete))
  :diminish
  :config (global-company-mode)
  (defun ajv/company-idle-delay ()
    (if (eq major-mode
	    'inferior-python-mode)
	nil 0.1))
  (setq company-dabbrev-downcase nil)
  (setq company-idle-delay 'ajv/company-idle-delay)
  (setq company-minimum-prefix-length 3)
  (setq company-selection-wrap-around t)


  ;; (add-hook 'company-mode-hook (lambda ()
  ;; 				 (substitute-key-definition 'company-complete-common
  ;; 							    'company-yasnippet-or-completion
  ;; 							    company-active-map)))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; From here. Dated 2015, tested 2023. Based on PR accepted into spacemacs
  ;; https://stackoverflow.com/a/28510968.
  ;; Add yasnippet support for all company backends
  ;; https://github.com/syl20bnr/spacemacs/pull/179
  (defvar company-mode/enable-yas t
    "Enable yasnippet for all backends.")

  (defun company-mode/backend-with-yas (backends)
    (if (or (not company-mode/enable-yas) (and (listp backends) (member 'company-yasnippet backends)))
	backends
      (append (if (consp backends) backends (list backends))
              '(:with company-yasnippet))))

  (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))

  ;; From here. Dated 2015, tested 2023. API use confirmed by author of yasnippet
  ;; https://stackoverflow.com/a/28510968
  ;; Try yas-expand and on failure to company-completion
  (defun company-yasnippet-or-completion ()
    (interactive)
    (let ((yas-fallback-behavior nil))
      (unless (yas-expand)
	(call-interactively #'company-complete-common))))

  ;; (add-hook 'company-mode-hook (lambda ()
  ;; 				 (substitute-key-definition 'company-complete-common
  ;; 							    'company-yasnippet-or-completion
  ;; 							    company-active-map)))
  :bind ((:map company-active-map)
	 ([remap company-complete-common] . company-yasnippet-or-completion))
  )

(use-package company-auctex
  :after company
  :mode ("\\.tex\\'" . LaTeX-mode)
  :config (company-auctex-init))



(use-package flyspell :diminish ""
  :commands flyspell-mode
  :config
  (setq flyspell-issue-welcome-flag nil)
  (setq flyspell-issue-message-flag nil)
  (setq ispell-program-name "aspell")    ; use aspell instead of ispell
  (unbind-key "C-;" flyspell-mode-map)
  :bind (("<mouse-3>" . flyspell-correct-word)
	 :map flyspell-mode-map
	 (("C-M-i" . nil)
	  ("C-." . flyspell-auto-correct-word)))
  :hook ((markdown-mode-hook text-mode-hook LaTeX-mode-hook org-mode-hook) . flyspell-mode))

(use-package expand-region
  :bind ("C-=" . er/expand-region))


(use-package ialign :demand)


(use-package smartparens-config :demand
  :straight smartparens
  :delight smartparens-mode "" "smartparens"
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

(use-package volatile-highlights :diminish
  :config (volatile-highlights-mode t))

(use-package god-mode :demand
  :init
  (setq god-mode-enable-function-key-translation nil)
  :bind (("<escape>" . god-mode-all)
	 (:map god-local-mode-map
	       ("." . repeat)
	       ("i" . god-mode-all)
	       ("<escape>" . (lambda () (interactive) (god-mode-activate)))))
  :hook ((god-mode-enabled-hook . ajv/god/god-mode-has-priority)
	 (god-mode-enabled-hook . ajv/god/update-cursor)
	 (god-mode-disabled-hook . ajv/god/update-cursor))
  :config
  (use-package ajv-god :demand
    :straight nil
    :bind ((:map god-local-mode-map
		 ("q" . ajv/god/insert-string-from-god-mode)))
    :config
    (setq god-exempt-major-modes
	  (append ajv/god/exempt-modes god-exempt-major-modes))
    (ajv/god/update-cursor)))

(use-package god-mode-isearch :after (god-mode)
  :straight god-mode
  :bind ((:map isearch-mode-map ("<escape>" . god-mode-isearch-activate))
	 (:map god-mode-isearch-map ("<escape>" . god-mode-isearch-disable))))

(use-package which-key :diminish
  :delight which-key-mode "" "which-key"
  :config
  (which-key-mode)
  (which-key-enable-god-mode-support))

(use-package aggressive-indent :diminish ""
  :config
  (add-to-list 'aggressive-indent-excluded-modes 'Messages)
  (global-aggressive-indent-mode 1))

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

(use-package highlight-indentation :diminish ""
  :config (diminish 'highlight-indentation-current-column-mode)
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

(use-package python
  :straight elpy
  :mode ("\\.py\\'" . python-mode)
  ;; :interpreter ("python" . python-mode)
  :commands python-mode
  :delight python-mode
  :config
  (use-package elpy
    :commands elpy-enable
    :init (advice-add 'python-mode :before 'elpy-enable)
    :bind
    ((:map elpy-mode-map
	   ("s-c" . elpy-shell-send-region-or-buffer)
	   ("s-C" . (lambda () (interactive)
		      (let ((current-prefix-arg '-)) ;; emulate C-u
			(call-interactively 'elpy-shell-send-region-or-buffer))))
	   ("C-c <" . indent-tools-hydra/body)))
    :config
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
    :hook ((elpy-mode-hook . (lambda () (elpy-shell-toggle-dedicated-shell 1)))))

  ;; This function is required so that Jupyter properly handles interactive plotting.
  ;; Otherwise it defaults to inline plotting which doesn't work as well in Emacs.
  ;; This is added as a hook to
  ;; (defun elpy-shell--use-interactive-plots-in-jupyter ()
  ;;   "Make sure we use an interactive backend with Jupyter"
  ;;   )

  ;; (defun elpy-shell--use-interactive-plots-in-jupyter ()
  ;;   "Make sure we use an interactive backend with Jupyter"
  ;;   (when (not (null (string-match "jupyter" python-shell-interpreter)))
  ;;     (let ((process (python-shell-get-process)))
  ;; 	(python-shell-send-string-no-output "%matplotlib qt") ;QtAgg
  ;; 	process)))
  ;; (add-hook 'python-shell-first-prompt-hook 'elpy-shell--use-interactive-plots-in-jupyter t)

  ;; :hook ((python-shell-first-prompt-hook . elpy-shell--use-interactive-plots-in-jupyter))
  )

(use-package flycheck :demand :after python :diminish
  :hook ((elpy-mode-hook . flycheck-mode)))

(use-package py-autopep8
  :hook ((elpy-mode-hook . py-autopep8--enable)))
;; ((elpy-mode-hook . py-autopep8-enable-on-save))

(use-package ein
  :bind ((:map ein:notebook-mode-map ("s-s" . ein:notebook-save-notebook-command-km)))
  :hook ((ein:markdown-mode-hook . ajv/company/disable)
	 (ein:notebook-mode-hook . ajv/company/disable)))

(use-package haskell-mode
  :mode "\\.hs\\'"
  :bind ((:map haskell-mode-map
	       ("M-." . haskell-mode-jump-to-def-or-tag)))
  :config
  ;; (require 'haskell-interactive-mode)
  ;; (require 'haskell-process)
  (setq haskell-process-path-ghci "/home/akshar/.ghcup/bin/ghci")
  (setq haskell-process-path-cabal "/home/akshar/.ghcup/bin/cabal")
  (setq haskell-process-type 'auto)
  (setq haskell-stylish-on-save t)
  (setq haskell-tags-on-save t)
  :hook (;; (haskell-mode-hook . haskell-interactive-mode)
	 (haskell-mode-hook . haskell-indentation-mode)
	 (haskell-mode-hook . (lambda ()
				(set (make-local-variable 'company-backends)
				     (append '((company-capf company-dabbrev-code))
					     company-backends))))))


;; (use-package flymake
;;   ;; TODO: Change this to flycheck
;;   ;; https://www.masteringemacs.org/article/spotlight-flycheck-a-flymake-replacement
;;   :commands flymake-mode
;;   :init
;;   (setq flymake-suppress-zero-counters t)
;;   (setq flymake-start-on-flymake-mode t)
;;   (setq flymake-no-changes-timeout nil)
;;   (setq flymake-start-on-save-buffer t)
;;   (setq flymake-proc-compilation-prevents-syntax-check t)
;;   :config
;;   (defun ajv/flymake/run () (interactive)
;; 	 (flymake-mode 1) (flymake-start) (flymake-show-diagnostics-buffer))
;;   (defun ajv/flymake/stop () (interactive)
;; 	 (flymake-stop-all-syntax-checks)
;; 	 (kill-buffer (flymake--diagnostics-buffer-name))
;; 	 (flymake-mode 0))
;;   (defun ajv/flymake/next-line-and-show () (interactive)
;; 	 (forward-button 1 nil nil nil) (flymake-show-diagnostic (point)))
;;   (defun ajv/flymake/previous-line-and-show () (interactive)
;; 	 (forward-button -1 nil nil nil) (flymake-show-diagnostic (point)))
;;   :bind (:map flymake-diagnostics-buffer-mode-map
;; 	      ("n" . ajv/flymake/next-line-and-show)
;; 	      ("p" . ajv/flymake/previous-line-and-show))
;;   :hook (flymake-diagnostics-buffer-mode-hook . visual-line-mode))

;; (use-package flymake-proselint :after flymake
;;   :init
;;   (dolist (mode '("markdown-mode" "text-mode"))
;;     (add-hook (intern (concat mode "-hook")) #'flymake-proselint-setup)))


(use-package ajv-sonic-pi
  :straight nil
  :commands (ajv/sonic-pi/filename-extension-spi-p ajv/sonic-pi/initialize-mode)
  :init
  ;; Because use-package's :magic keyword doesn't yet allow use of match functions
  (add-to-list 'magic-mode-alist
	       '(ajv/sonic-pi/filename-extension-spi-p . ajv/sonic-pi/initialize-mode))
  :config
  (setq sonic-pi-path "/usr/lib/sonic-pi/server/bin/")
  (setq sonic-pi-server-bin "sonic-pi-server.rb")
  (add-to-list 'company-backends '(ajv/sonic-pi/company-backend))
  :bind
  ((:map sonic-pi-mode-map
	 ("C-c C-l" . sonic-pi-send-live-loop)
	 ("C-c C-c" . sonic-pi-send-buffer)
	 ("C-c C-k" . sonic-pi-stop-all))))

;; (use-package tidal)


(use-package move-text :config (move-text-default-bindings))

(use-package iedit)

(use-package hungry-delete :diminish
  :config
  (global-hungry-delete-mode)
  (setq hungry-delete-join-reluctantly t))

(use-package ws-butler :diminish :delight :config (ws-butler-global-mode))

(use-package dumb-jump :diminish :delight
  :config
  (setq xref-show-definitions-function #'xref-show-definitions-completing-read)
  (setq dumb-jump-prefer-searcher 'rg)
  :hook (xref-backend-functions . dumb-jump-xref-activate))



(use-package rg :commands (rg-menu)
  :bind (("C-c s" . rg-menu)
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

(use-package undo-tree :demand :diminish
  :config
  (setq undo-tree-auto-save-history nil)
  (global-undo-tree-mode))

(use-package hide-mode-line
  :config
  (ajv/make-enable-disable-defuns hide-mode-line-mode mode-line  1 -1))

(use-package pdf-tools :magic ("%PDF" . pdf-view-mode)
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
    :straight nil
    :bind (:map pdf-view-mode-map
		("M-m" . ajv/pdf-tools/toggle-modeline)))
  (when (featurep 'ido)
    (advice-add #'ido-find-file :filter-return #'ajv/pdf-tools/launch-file))
  (setq pdf-view-resize-factor 1.05)
  (setq auto-revert-interval 0.1)
  (setq auto-revert-verbose nil)
  :hook ((pdf-view-mode-hook . ajv/mode-line/disable)
	 (pdf-view-mode-hook . ajv/pdf-tools/save-disable-modeline-format)
	 (pdf-view-mode-hook . ajv/pdf-tools/disable-linum-mode)
	 (pdf-view-mode-hook . auto-revert-mode)
	 (pdf-view-mode-hook . (lambda () (if git-gutter-mode (git-gutter-mode -1) nil)))
	 (pdf-view-mode-hook . pdf-misc-size-indication-minor-mode))
  )

;; This is NOT needed anymore since some 2022 May update.
;; If normal pdf-tools-intall when doing epdfinfo needs reinstall throws a poppler error,
;; Then, add `CXXFLAGS='-std=c++17'' as an option in the
;; ./configure -q $(quote "$prefix") && make clean && make -s || exit_fail
;; command inside autobuild in the `/elpa/pdf-tools-20220426.2203/build/server'
;; That resolves the issues, although not very satisfactorily.


(use-package pdf-view-restore
  :after pdf-tools
  :config
  (setq pdf-view-restore-filename (concat user-emacs-directory ".pdf-view-restore"))
  (setq use-file-base-name-flag nil)
  ;; :config (setq pdf-view-restore-filename "~/.emacs.d/.pdf-view-restore")
  :hook (pdf-view-mode-hook . pdf-view-restore-mode))


(use-package nov :mode ("\\.epub\\'" . nov-mode)
  :bind (:map nov-mode-map
	      ("j" . nov-scroll-up)
	      ("k" . nov-scroll-down)
	      ("f" . nov-history-forward)
	      ("b" . nov-history-back)
	      ("q" . ajv/nov/quit)
	      ("M-m" . ajv/nov/toggle-modeline))
  :config
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
  (setq nov-text-width 80)
  (use-package ajv-nov :demand
	       :straight nil
	       )
  :hook (;; (change-major-mode-after-body-hook . ajv/nov/dedicated-frame-on-mode-start)
	 (nov-mode-hook . ajv/nov/dedicated-frame-on-mode-start)
	 (nov-mode-hook . ajv/nov/font-setup)
	 (nov-mode-hook . ajv/nov/save-disable-modeline-format)
	 (nov-post-html-render-hook . ajv/nov/post-html-render-margin-adjustments)))

;; Mode for .gitignore, .git/info/exclude, and git/ignore files.
(use-package gitignore-mode
	     :straight git-modes
	     )

;; Mode for .gitconfig, .git/config, git/config, and .gitmodules files.
(use-package gitconfig-mode
	     :straight git-modes
	     )

;; Mode for .gitattributes, .git/info/attributes, and git/attributes files.
(use-package gitattributes-mode
	     :straight git-modes
	     )

(use-package magit
  :bind (("<f2>" . magit-status)
	 (:map ido-common-completion-map
	       ("<f2>" . ido-enter-magit-status))
	 :map magit-status-mode-map
	 ([remap magit-mode-bury-buffer] . ajv/magit/kill-buffers))
  :config
  (defun ajv/magit/kill-buffers ()
    "Restore window configuration and kill all Magit buffers.
Taken from: http://manuel-uberti.github.io/emacs/2018/02/17/magit-bury-buffer/"
    (interactive)
    (let ((buffers (magit-mode-get-buffers)))
      (magit-restore-window-configuration)
      (mapc #'kill-buffer buffers)))
  ;; Not needed since I moved to helm.
  ;; (setq magit-completing-read-function 'magit-ido-completing-read)
  (setq magit-diff-refine-hunk t)
  ;; TODO: At some point figure out if we can provide a better location
  (transient-append-suffix 'magit-status-jump '(0 0 -1)
    '("c " "Commits" magit-jump-to-unpushed-to-upstream)))

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
  :straight nil
  :init (use-package git-gutter-fringe :delight git-gutter-mode)
  :config
  (setq git-gutter:disabled-modes '(pdf-view-mode))
  (global-git-gutter-mode t)
  (fringe-mode '(0 . nil)))

(use-package keychain-environment :after magit :config (keychain-refresh-environment))

(use-package github-explorer :commands github-explorer)


(use-package dired :demand :delight dired-mode "Dired"
  :straight nil
  :config
  (setq dired-dwim-target t)                     ;default copy to other window
  (setq dired-recursive-copies 'always)
  ;;allow 'a' in dired
  (put 'dired-find-alternate-file 'disabled nil))

(use-package dired-x :after dired :demand
  :straight (:type built-in)
  ;; Swap keybindings for dired-jump and dired-jump-other-window
  :bind (("C-x C-j" . dired-jump)
	 ("C-x M-j" . dired-jump-other-window))
  :config
  (add-to-list 'dired-guess-shell-alist-user '("\\.pdf\\'" "evince"))
  (add-to-list 'dired-guess-shell-alist-user '("\\.py\\'" "python"))
  (add-to-list 'dired-guess-shell-alist-user '("\\.hy\\'" "hy")))

(use-package dired-narrow :after dired
  :bind (:map dired-mode-map ("/" . dired-narrow-regexp)))

(use-package ajv-dired :demand :after dired
  :straight nil
  :bind  (:map dired-mode-map
	       ("s". ajv/dired/sort-criteria)
	       ("l" . ajv/dired/launch-file)
	       ("C-c C-d C-b" . ajv/dired/delete-backup-files)
	       ("W" . ajv/dired/copy-directory-name-as-kill)
	       ("e" . dired-toggle-read-only) ;short binding for C-x C-q
	       ("q" . dired-toggle-read-only) ;consistency to god-mode binding
	       (")" . ajv/dired/toggle-symlink-dereferencing)
	       ("C-c C-c" . compile)
	       ("s-c" . compile)
	       ("s-c" . compile)
	       ;; ("M-<" . ajv/dired/go-to-beginning-of-buffer)
	       ;; ("M->" . ajv/dired/go-to-end-of-buffer)
	       )
  :config
  (setq dired-listing-switches ajv/dired/listing-switches-without-symlink)
  :hook ((dired-mode-hook . ajv/dired/set-default-sorting)
	 (dired-mode-hook . ajv/dired/hide-details-omit-hidden-files)
	 (dired-mode-hook . ajv/hl-line/enable)))

(use-package wdired :after dired
  :config
  (setq wdired-use-dired-vertical-movement 'sometimes)
  (advice-add 'wdired-finish-edit :after #'ajv/hl-line/enable)
  :hook (wdired-mode-hook . ajv/hl-line/disable))

(use-package dired-rainbow :after dired
  :config (use-package ajv-dired-rainbow
		       :straight nil
		       ))

(use-package dired-collapse :demand :after dired)

(use-package dired-subtree :demand :after dired
  :config
  (setq dired-subtree-use-backgrounds nil)
  :bind (:map dired-mode-map
              ("<tab>" . dired-subtree-toggle)
              ("<C-tab>" . dired-subtree-cycle)
              ("<S-iso-lefttab>" . dired-subtree-remove)
	      ("f" . dired-subtree-next-sibling)
	      ("b" . dired-subtree-previous-sibling)))

;; (use-package projectile
;;   :init (projectile-mode +1)
;;   :config
;;   (use-package helm-projectile)
;;   (helm-projectile-on)
;;   (setq projectile-project-search-path '(("~/0/source/" . 1) ("~/0/research/" . 2)))
;;   (setq projectile-sort-order 'recently-active)
;;   (setq projectile-switch-project-action #'projectile-dired)
;;   :bind (:map projectile-mode-map
;; 	      ("s-p" . projectile-command-map)
;; 	      ("C-c p" . projectile-command-map)))

(use-package make-it-so :config (mis-config-default))

(use-package ajv-org :demand
  :straight nil
  :init
  (use-package org
	       :straight (:type built-in)
	       )
  (use-package ol-notmuch
	       :straight (:type git :host github :repo "emacsmirror/ol-notmuch")
	       )
  :bind
  (("C-c a" . org-agenda)
   ("s-a" . org-agenda-list)
   ("C-c C-g" . org-goto)		;because org-journal overrides this.
   ("C-c l" . org-store-link)
   ("C-c c" . org-capture)
   ("C-c C-<return>" . ajv/org/open-link-in-current-window)
   ("<f10>" . (lambda () (interactive) (delete-other-windows) (switch-to-buffer "*Org Agenda*"))))
  :config
  (setq org-agenda-files ajv/settings/my-org-agenda-files)
  (setq org-modules '(ol-bbdb ol-bibtex ol-docview ol-gnus org-habit ol-info ol-irc
			      ol-mhe ol-rmail ol-w3m org-tempo))
  (setq org-directory ajv/sensitive/my-org-agenda-files-dir)
  (setq org-default-notes-file (concat ajv/sensitive/my-org-agenda-files-dir "refile.org"))
  (setq ajv/settings/surfing-journal (concat ajv/sensitive/my-org-agenda-files-dir "surfing.org"))

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)
     (latex . t)))

  (setq org-confirm-babel-evaluate t)

  (add-to-list 'org-structure-template-alist '("sh" . "src bash"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("()" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("py" . "src python"))

  ;; (defun ajv/org-agenda/truncation ()
  ;;   (interactive)
  ;;   )

  ;; :hook ((org-agenda-mode-hook . ajv/org-agenda/truncation))
  )

(use-package org-tempo
  :straight nil
  )

(use-package org-bullets :hook ((org-mode-hook . org-bullets-mode)))

(use-package org-noter :commands (org-noter)
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

(use-package org-journal
  :init
  ;; Change default prefix key; needs to be set before loading org-journal
  (setq org-journal-prefix-key "C-c j ")
  ;; :bind (:map org-mode-map ("C-c C-j" . org-journal-new-entry))
  :bind (("C-c C-j" . org-journal-new-entry))
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
  (use-package ajv-latex :demand t
    :straight nil
    :config
    (use-package reftex-ref
      :config
      ;; Don't show reftex highlight in modeline
      (delight 'reftex-mode nil "reftex")
      (setq reftex-ref-style-default-list '("Cleveref")))
    :bind (:map reftex-mode-map
		("C-c )" . ajv/latex/reftex-reference-cleveref-wrapper)))


  :bind ((:map LaTeX-mode-map
	       ("<f5>" . TeX-command-run-all)
	       ("s-c" . TeX-command-run-all)
	       ("s-C" . TeX-command-master)
	       ("<f6>" . TeX-next-error)
	       ;; Map dollar to self-insert-command to ensure that smartparens works.
	       ;; As suggested here: https://github.com/Fuco1/smartparens/issues/834
	       ("$" . self-insert-command)))
  :hook ((LaTeX-mode-hook . turn-on-reftex)
	 (LaTeX-mode-hook . TeX-source-correlate-mode))
  )

(use-package markdown-toc :commands markdown-toc-mode
  :hook (markdown-mode-hook . markdown-toc-mode))

;; (use-package polymode)
(use-package poly-org :commands poly-org-mode)
;; :config
;; (fmakunbound 'poly-org-mode-matcher)
;; (fset 'poly-org-mode-matcher #'(lambda () (first (org-babel-get-src-block-info))))
;; )

(use-package poly-markdown :commands poly-markdown-mode)

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
  :straight nil
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

(use-package buffer-move :bind (("<f11>" . buf-move-left) ("<f12>" . buf-move-right)))

(use-package ajv-scpaste
  :straight nil
  :commands (ajv/scpaste/paste-region-or-buffer))

(use-package monokai-pro-theme)

(use-package doom-modeline
  :config (doom-modeline-mode 1)
  (setq doom-modeline-bar-width 3)
  (setq doom-modeline-hud nil)
  (setq doom-modeline-display-misc-in-all-mode-lines t)

  ;; This causes the current time in the mode line to be displayed in
  ;; `ajv/settings/display-time-face' to make it stand out visually.
  (setq display-time-string-forms
        '((propertize (concat " " 24-hours ":" minutes "")
                      'face 'ajv/settings/display-time-face)))

  (setq display-time-24hr-format t)       ;Use 24 hr format
  (setq display-time-day-and-date t)      ;Also show day and date
  (display-battery-mode 1)
  (display-time-mode 1)
  (setq mode-line-compact 'long))

(use-package ajv-theme :demand
  :straight nil
  :config
  (setq custom-theme-allow-multiple-selections t)

  (load-theme ajv/settings/prefered-light-theme-name t t)
  (load-theme ajv/settings/prefered-dark-theme-name t nil)
  ;; (unless (member ajv/settings/prefered-dark-theme-name custom-enabled-themes)
  ;;   (enable-theme ajv/settings/prefered-dark-theme-name)) ;enable if not already enabled
  (ajv/theme/set-region-face t)


  (setq-default fill-column most-positive-fixnum)
  (setq-default visual-line-fringe-indicators '(nil right-curly-arrow))
  (setq visual-fill-column-center-text t)
  (setq visual-fill-column-width 110)

  ;; The hostname part in `frame-title-format' taken from:
  ;; https://blog.lambda.cx/posts/eamacs-improved-frame-title/
  (setq-default frame-title-format '("%b @" (:eval (or (file-remote-p default-directory 'host) system-name)) " - Emacs"))
  (column-number-mode t)
  (set-fringe-style '(0 . nil))
  (add-to-list 'default-frame-alist '(fullscreen . fullboth)) ;maximize all frames
  (add-to-list 'default-frame-alist `(font . ,ajv/settings/prefered-font-name))
  :hook ((text-mode-hook . turn-on-auto-fill))
  )

;; (use-package unicode-fonts :config (unicode-fonts-setup))

;; (use-package fancy-battery ;; :demand
;;   :config
;;   (setq fancy-battery-show-percentage t)
;;   ;;    ;; #121212 is gray7 in list-colors-display
;;   ;;    ;; #7fff00 is chartreuse in list-colors-display
;;   ;;    ;; #ff0000 is red in in list-colors-display
;;   ;;    ;; #00bfff is DeepSkyBlue in list-colors-display
;;   (set-face-attribute 'fancy-battery-critical nil
;; 		      :inherit '(bold)
;; 		      :foreground "#ff0000")
;;   (set-face-attribute 'fancy-battery-charging nil
;; 		      :inherit '(bold)
;; 		      :foreground "#00bfff")
;;   (set-face-attribute 'fancy-battery-discharging nil
;; 		      :inherit '(bold)
;; 		      :foreground "#7fff00")
;;   ;; :background "#121212"
;;   ;; (fancy-battery-mode 1)
;;   )

;; (use-package ajv-modeline :demand :straight nil
;;   :config
;;   (setq display-time-string-forms
;; 	'((propertize (concat " " 24-hours ":" minutes "")
;; 		      'face 'ajv/settings/display-time-face)))

;;   (setq display-time-24hr-format t)       ;Use 24 hr format
;;   (setq display-time-day-and-date t)      ;Also show day and date

;;   (setq mode-line-compact 'long)
;;   (display-time-mode 1)                   ;Actually show the time
;;   (force-mode-line-update)
;;   )


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
	      ("q" . (lambda () (interactive) (ajv/kill-this-buffer) (delete-frame)))))

(use-package keycast :commands (keycast-mode))

;; ;; (use-package memento-mori
;; ;;   :config
;; ;;   (setq memento-mori-birth-date ajv/sensitive/my-birthdate)
;; ;;   (memento-mori-mode))

;; The article: You’re probably using the wrong dictionary
;; Link: http://jsomers.net/blog/dictionary
;; Getting this to work in Emacs via sdcv and sdcv-mode
;; http://mbork.pl/2017-01-14_I'm_now_using_the_right_dictionary
;; Source for some other dictionaries in the stardict format
;; http://download.huzheng.org/bigdict/
(use-package sdcv-mode
  :straight nil
  )

(when use-package-compute-statistics
  (use-package-report)
  ;; To sort the use-package statistics buffer by loading time
  (with-current-buffer "*use-package statistics*"
    (progn (tabulated-list-sort 3)
	   (tabulated-list-sort 3)
	   (beginning-of-buffer))))
