(provide 'ajv-notmuch)

(setq-default notmuch-search-oldest-first nil)
(setq mm-text-html-renderer 'w3m)
(setq mm-default-directory "~/Downloads/")
(setq mm-html-inhibit-images t)
(setq notmuch-multipart/alternative-discouraged '("text/plain" "text/html"))

(setq notmuch-saved-searches
      (quote
       ((:name "unread" :query "tag:unread" :key "u" :search-type tree)
	(:name "main-gmail" :query "path:main-gmail/**" :key "m" :search-type tree)
	(:name "neu-email" :query "path:neu-email/**" :key "n" :search-type tree)
	(:name "all mail" :query "*" :key "a" :search-type tree)
	(:name "replied" :query "tag:replied" :key "r" :search-type tree)
	(:name "1 days" :query "date:1D.." :key "1" :search-type tree)
        (:name "2 days" :query "date:2D.." :key "2" :search-type tree)
        (:name "5 days" :query "date:5D.." :key "5" :search-type tree)
	(:name "1 week" :query "date:1W.." :key "w" :search-type tree)
	(:name "sent" :query "tag:sent" :key "s" :search-type tree)
	(:name "flagged" :query "tag:flagged" :key "f" :search-type tree)
	(:name "drafts" :query "tag:draft" :key "d" :search-type tree))))

(defvar ajv/notmuch/timer-for-polling nil
  "A timer that repeatedly polls notmuch to see if there are new emails")

(defvar ajv/notmuch/period-for-updating-notmuch-in-seconds 600
  "The idle time, in seconds, after which to automatically show my preferred window configuration. Used in the ajv/settings/timer-to-periodically-show-window-config timer.")

(defvar ajv/notmuch/timer-to-periodically-update-notmuch nil
  "Contains a reference to a timer that periodically (period set in ajv/notmuch/period-for-updating-notmuch-in-seconds) shows my preferred window config in the notmuch frame (found in ajv/settings/notmuch-frame-reference).")


(defun ajv/notmuch/poll-quietly ()
  "Call notmuch-poll but inhibit messages so that it doesn't pollute minibuffer. Used primarily in the timer that polls at regularl intervals automatically, without manual intervnetion."
  (when (get-buffer "*notmuch-hello*")	;check if buffer exists
    (with-current-buffer "*notmuch-hello*"
      (let ((inhibit-message t))
	(notmuch-poll-and-refresh-this-buffer)))))

(defun ajv/notmuch/start-notmuch ()
  "Show (by creating if needed) the notmuch hello buffer"
  (interactive)
  (unless ajv/settings/notmuch-frame-reference
    (setq ajv/settings/notmuch-frame-reference (selected-frame)))
  (notmuch))

(defun ajv/notmuch/poll-and-refresh-quietly ()
  "Show (by creating if needed) the notmuch hello buffer"
  (interactive)
  (when (and ajv/settings/notmuch-frame-reference
	     (frame-live-p ajv/settings/notmuch-frame-reference)
	     (get-buffer "*notmuch-hello*"))
    (select-frame ajv/settings/notmuch-frame-reference)
    ;; (unless (get-buffer "*notmuch-hello*") ;create buffer (unless it exists)
    ;;   (notmuch))
    (with-current-buffer "*notmuch-hello*" ;poll and refresh notmuch
      (let ((inhibit-message t))
	(notmuch-poll-and-refresh-this-buffer)
	(ajv/notmuch/alert-update-mail-count-mode-line)
	(ajv/notmuch/set-initial-cursor-position)))))

(defun ajv/notmuch/tree-toggle-unread ()
  "While in notmuch-tree-mode, toggle unread tag"
  (interactive)
  (if (member "unread" (notmuch-tree-get-tags))
      (notmuch-tree-tag (list "-unread"))
    (notmuch-tree-tag (list "+unread")))
  (forward-line)
  )

(defun ajv/notmuch/show-toggle-unread ()
  "While in notmuch-show-mode, toggle unread tag"
  (interactive)
  (if (member "unread" (notmuch-show-get-tags))
      (notmuch-show-tag (list "-unread"))
    (notmuch-show-tag (list "+unread")))
  (forward-line)
  )

(defun ajv/notmuch/search-toggle-unread ()
  "While in notmuch-search-mode, toggle unread tag"
  (interactive)
  (if (member "unread" (notmuch-search-get-tags))
      (notmuch-search-tag (list "-unread"))
    (notmuch-search-tag (list "+unread")))
  (forward-line)
  )

(defun ajv/notmuch/set-initial-cursor-position ()
  (interactive)
  (beginning-of-buffer)
  (if (and (eq (point) (point-min))
           (search-forward "Saved searches:" nil t))
      (progn
        (forward-line)
        (widget-forward 1))
    (when (eq (widget-type (widget-at)) 'editable-field)
      (beginning-of-line))))


(defun ajv/notmuch/clear-searches ()
  (interactive)
  (setq notmuch-search-history nil)
  (notmuch-hello-update))


(defun ajv/notmuch/copy-link ()
  "Opens the current link or image or current page's uri or any url-like text under cursor in external browser."
  ;; http://blog.binchen.org/posts/open-url-in-emacs-with-external-browser.html
  (interactive)
  (let ((url (or (w3m-anchor) (w3m-image))))
    (if url
	(progn
	  (kill-new url)
	  (message (concat "Copied url.")))
      (message "No url like stuff detected"))))


(defun ajv/notmuch/unread-email-count ()
  "Counts the number of unread emails known to Notmuch."
  (interactive)
  (string-to-number (notmuch-saved-search-count "tag:unread")))

(defun ajv/notmuch/show-unread-email-count ()
  "Counts the number of unread emails known to Notmuch."
  (interactive)
  (message (ajv/notmuch/unread-email-count)))


;; Mode-line indicator for unread emails
;; This is modified from the mu4e-alert package's modeline features.
;; https://github.com/iqbalansari/mu4e-alert/blob/master/mu4e-alert.el
;; TODO: An alternative to consider might be to use the following existing functionality:
;; `display-time-mail-string', and other things in there to get a sense for how to make that work.
;; The unicode mail symbol was picked up from there.

(defvar ajv/notmuch/alert-mode-line nil "The mode-line indicator to display the count of unread emails.")

(defun ajv/notmuch/alert-default-mode-line-formatter (mail-count)
  "Default formatter used to get the string to be displayed in the mode-line.
MAIL-COUNT is the count of mails for which the string is to displayed"
  (when t ;; (not (zerop mail-count))
    (concat " "
            (propertize
             ""
	     ;; 'display nil
             ;; 'display (when (display-graphic-p)
             ;;            display-time-mail-icon)
             'face display-time-mail-face
             'help-echo (concat (if (= mail-count 1)
                                    "You have an unread email"
                                  (format "You have %s unread emails" mail-count))
                                "\nClick here to view "
                                (if (= mail-count 1) "it" "them"))
             'mouse-face 'mode-line-highlight
             'keymap '(mode-line keymap
                                 (mouse-1 . (notmuch-tree "tag:unread"))
                                 (mouse-2 . (notmuch-tree "tag:unread"))
                                 (mouse-3 . (notmuch-tree "tag:unread"))))
            ;; (format " ✉ [%d] " mail-count)
            (if (zerop mail-count)
                " "
              (format " ✉ [%d] " mail-count))
	    )))


(defun ajv/notmuch/alert-update-mail-count-mode-line ()
  "Send a desktop notification about currently unread email."
  (setq ajv/notmuch/alert-mode-line (ajv/notmuch/alert-default-mode-line-formatter
				     (ajv/notmuch/unread-email-count)))
  (force-mode-line-update))

(defun ajv/notmuch/alert-enable-mode-line-display ()
  (interactive)
  (add-to-list 'global-mode-string '(:eval ajv/notmuch/alert-mode-line)))

(defun ajv/notmuch/alert-disable-mode-line-display ()
  (interactive)
  (setq global-mode-string (delete '(:eval ajv/notmuch/alert-mode-line) global-mode-string)))


(defun ajv/notmuch/get-all-notmuch-addresses ()
  (interactive)
  (setq ajv/notmuch/all-notmuch-addresses-list (mapcar (lambda (string) (elt (reverse (split-string string "[<>]" t)) 0)) (hash-table-keys notmuch-address-completions))))

(defun ajv/notmuch/regexp-query-notmuch-addresses (query)
  (interactive "sRegexp: ")
  (unless ajv/notmuch/all-notmuch-addresses-list
    (ajv/notmuch/get-all-notmuch-addresses))
  (let* ((out-list nil)
	 (out-buf (generate-new-buffer "*addresses matching regexp*")))
    (progn (dolist (addr ajv/notmuch/all-notmuch-addresses-list)
	     (when (string-match query addr)
	       (push addr out-list)))
	   (princ out-list out-buf)
	   (switch-to-buffer out-buf)
	   out-list)))


(defun ajv/notmuch/flush-msmtpq ()
  (interactive)
  (start-process "pymsmtpq-flush" "*notmuch-pymsmtpq*"
		 "msmtpq-send-and-notify.sh"))
