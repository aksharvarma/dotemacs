;; Highlights the line the cursor is currently on. (1/0 bool)
(global-hl-line-mode 0)

;; (load-theme 'wombat)
;; (load-theme 'misterioso)
;; (load-theme 'manoj-dark)

;; The following was picked up from: https://stackoverflow.com/questions/9446673/asking-emacs-to-highlight-more-clearly-which-window-pane-has-the-focus-cursor
;; These need to be called again after changing the theme.
(defun ajv-set-modeline-color-scheme ()
  "Make active modeline more visible and inactive one less visible"
  (interactive nil)
  (progn
    (set-face-attribute  'mode-line
                         nil 
                         :foreground "black"
                         :background "gray80"
                         :box '(:line-width 1 :style released-button))
    (set-face-attribute  'mode-line-inactive
                         nil 
                         :foreground "gray50"
                         :background "grey20"
                         :box '(:line-width 1 :style released-button))))


(defun ajv-set-theme (chosen-theme)
  "Set the theme, modeline color scheme and cursor color to what I want."
  (interactive S)
  (progn
    (enable-theme chosen-theme)
    (ajv-set-modeline-color-scheme)
    ;; (set-cursor-color "yellow")
    ))


;; (load-theme 'tango-dark)
;; (load-theme 'tsdh-dark)
(load-theme 'deeper-blue)
;; (load-theme 'manoj-dark)

;; (ajv-set-theme 'deeper-blue)
;; (ajv-set-theme 'manoj-dark)

;; (defun ajv-cycle-theme ()
;;   "Cycle Emacs theme among a preset list.
;; Code modified from: `http://ergoemacs.org/emacs/elisp_toggle_command.html'
;; Version 2018-04-25"
;;   (interactive nil)
;;   ;; uses a property “state”. Value is a integer.
;;   (let* ((values [misterioso wombat manoj-dark])
;;          ;; (values ["wombat" "manoj-dark" "misterioso"])
;;          (index-before (if (get 'ajv-cycle-theme 'current-theme)
;;                            (get 'ajv-cycle-theme 'current-theme) 0))
;;          (index-after (% (+ index-before 1)
;;                           (length values)))
;;          (next-value (aref values index-after)))

;;     (progn
;;       (put 'ajv-cycle-theme 'current-theme index-after)
;;       (ajv-set-theme next-value)
;;       (message "Theme changed to %s" next-value))))

;; Default theme is wombat for now. Can cycle through using the function above.
;; (ajv-set-theme 'wombat)


;; This is a complicated way to make the cursor color be something at the daemon level.
;; (require 'frame)
;; (defun set-cursor-hook (frame)
;; (modify-frame-parameters
;;   frame (list (cons 'cursor-color "yellow"))))

;; (add-to-list 'default-frame-alist '(cursor-color . "yellow"))

;; (add-hook 'after-make-frame-functions 'set-cursor-hook)
;; (add-hook 'after-make-frame-functions 'ajv-window-config)
;; (add-hook 'after-make-frame-functions
;;           (lambda (frame)
;;             (with-selected-frame frame
;;               (ajv-window-config))))

;; (add-hook 'after-setting-font-hook
;;           (lambda (frame)
;;             (with-selected-frame frame
;;               (set-cursor-color "yellow"))))

;; How I want my frames to look when I start emacs.
;; (defun ajv-window-config ()
;;   "Sets windows according to my liking"
;;   (interactive)
;;   ;; (delete-other-windows)
;;   (split-window-horizontally)
;;   (other-window 1)
;;   (switch-buffer-scratch)
;;   (other-window 1)
;;   (ajv-set-theme 'wombat)
;;   (dired "~/"))

;; Putting hooks so that the correct things are called after frames are created.
;; (if (daemonp)
;;     (add-hook 'after-make-frame-functions
;;               (lambda (frame)
;;                 (with-selected-frame frame
;;                   (ajv-window-config))))
;;   (ajv-window-config))
