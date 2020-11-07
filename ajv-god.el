(provide 'ajv-god)

(defun ajv/god/update-cursor ()
  (if (bound-and-true-p god-local-mode)
      (set-cursor-color "dark orange")
    (cond ((ajv/using-dark-theme-p) (set-cursor-color "lime green"))
	  ((ajv/using-light-theme-p) (set-cursor-color "black")))))

(setq ajv/god/exempt-modes '(org-agenda-mode elfeed-search-mode notmuch-hello-mode notmuch-search-mode notmuch-show-mode notmuch-tree-mode))

(defun ajv/god/insert-string-from-god-mode (string)
  "Read a string from the minibuffer and then insert it at point while in God mode.

 Useful for when minor insertions are needed but you don't want to leave God mode only to re-enter after typing just a few characters."
  (interactive "sString: ")
  ;; When region is active, delete the region before inserting text.
  (when (region-active-p)
    (delete-region (region-beginning) (region-end)))
  ;; Insert the text read in from the minibuffer
  (insert string))


;; NOTE: I haven't started using this yet.
;; This is supposed to go on god-mode-enabled-hook
(defun ajv/god/god-mode-has-priority ()
  "
Try to ensure that god mode keybindings retain priority over other minor modes.

Taken from: https://github.com/emacsorphanage/god-mode/issues/49
which itself is inspired by: https://stackoverflow.com/a/5340797
"
  (unless (and (consp (car minor-mode-map-alist))
               (eq (caar minor-mode-map-alist) 'god-local-mode-map))
    (let ((godkeys (assq 'god-local-mode minor-mode-map-alist)))
      (assq-delete-all 'god-local-mode minor-mode-map-alist)
      (add-to-list 'minor-mode-map-alist godkeys))))
