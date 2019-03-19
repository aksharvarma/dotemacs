(provide 'ajv-god)

(defun ajv/god-update-cursor ()
  (if (bound-and-true-p god-local-mode)
      (set-cursor-color "dark orange")
    (set-cursor-color "lime green"))
  )

(setq ajv/god-exempt-modes '(org-agenda-mode elfeed-search-mode notmuch-hello-mode notmuch-search-mode notmuch-show-mode))
