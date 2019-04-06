(provide 'ajv-god)

(defun ajv/god-update-cursor ()
  (if (bound-and-true-p god-local-mode)
      (set-cursor-color "dark orange")
    (set-cursor-color "lime green"))
  )

(setq ajv/god-exempt-modes '(org-agenda-mode elfeed-search-mode notmuch-hello-mode notmuch-search-mode notmuch-show-mode))

(defun ajv/insert-string-from-god-mode (string)
  "Read a string from the minibuffer and then insert it at point while in God mode.

 Useful for when minor insertions are needed but you don't want to leave God mode only to re-enter after typing just a few characters."
  (interactive "sString: ")
  ;; When region is active, delete the region before inserting text.
  (when (region-active-p)
    (delete-region (region-beginning) (region-end)))
  ;; Insert the text read in from the minibuffer
  (insert string))
