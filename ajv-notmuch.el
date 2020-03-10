(provide 'ajv-notmuch)

(setq notmuch-search-oldest-first nil
      mm-text-html-renderer 'w3m
      mm-default-directory "~/Downloads/"
      mm-html-inhibit-images t
      notmuch-multipart/alternative-discouraged '("text/plain" "text/html"))

(setq notmuch-saved-searches
      (quote
       ((:name "unread" :query "tag:unread" :key "u" :search-type tree)
	(:name "main-gmail" :query "path:main-gmail/**" :key "m" :search-type tree)
	(:name "neu-email" :query "path:neu-email/**" :key "n" :search-type tree)
	(:name "all mail" :query "*" :key "a" :search-type tree)
	(:name "1 days" :query "date:1D.." :key "1" :search-type tree)
        (:name "2 days" :query "date:2D.." :key "2" :search-type tree)
        (:name "5 days" :query "date:5D.." :key "5" :search-type tree)
	(:name "1 week" :query "date:1W.." :key "w" :search-type tree)
	(:name "sent" :query "tag:sent" :key "s" :search-type tree)
	(:name "flagged" :query "tag:flagged" :key "f" :search-type tree)
	(:name "drafts" :query "tag:draft" :key "d" :search-type tree))))

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


(defun ajv/notmuch/clear-searches ()
  (interactive)
  (setq notmuch-search-history nil)
  (notmuch-hello-update))
