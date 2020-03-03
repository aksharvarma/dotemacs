(provide 'ajv-notmuch)

(setq notmuch-search-oldest-first nil
      mm-text-html-renderer 'w3m
      mm-default-directory "~/Downloads/"
      mm-html-inhibit-images t
      notmuch-multipart/alternative-discouraged '("text/plain" "text/html"))

(setq notmuch-saved-searches
      (quote
       ((:name "unread" :query "tag:unread" :key "u")
	(:name "main-gmail" :query "path:main-gmail/**" :key "m")
	(:name "neu-email" :query "path:neu-email/**" :key "n")
	(:name "all mail" :query "*" :key "a")
	(:name "sent" :query "tag:sent" :key "t")
	(:name "flagged" :query "tag:flagged" :key "f")
	(:name "drafts" :query "tag:draft" :key "d"))))

(defun ajv/notmuch-show-toggle-unread ()
  "While in notmuch-show-mode, toggle unread tag"
  (interactive)
  (if (member "unread" (notmuch-show-get-tags))
      (notmuch-show-tag (list "-unread"))
    (notmuch-show-tag (list "+unread")))
  )

(defun ajv/notmuch-search-toggle-unread ()
  "While in notmuch-search-mode, toggle unread tag"
  (interactive)
  (if (member "unread" (notmuch-search-get-tags))
      (notmuch-search-tag (list "-unread"))
    (notmuch-search-tag (list "+unread")))
  (forward-line)
  )

(defun ajv/notmuch-set-initial-cursor-position ()
  (if (and (eq (point) (point-min))
           (search-forward "Saved searches:" nil t))
      (progn
        (forward-line)
        (widget-forward 1))
    (if (eq (widget-type (widget-at)) 'editable-field)
        (beginning-of-line))))
