(provide 'ajv-elfeed)

(defun ajv/kill-elfeed-log-buffer ()
  "While in notmuch-show-mode, toggle unread tag"
  (interactive)
  (kill-matching-buffers "\*elfeed-log\*" nil t)
  )

(defun ajv/elfeed-kill-buffers ()
  "Kill all elfeed buffers.
Taken from: http://manuel-uberti.github.io/emacs/2018/02/17/magit-bury-buffer/"
  (interactive)
  (let ((buffers (list "\*elfeed-search\*" "\*elfeed-log\*" "elfeed-blogs\.org")))
    (mapc (lambda (buffer-name) (kill-matching-buffers buffer-name nil t)) buffers)))

;; (defun ajv/elfeed-mode-local-hook-to-kill-buffers ()
;;   "A local hook that will be added onto elfeed-search-mode-hook"
;;   (add-hook 'kill-buffer-hook 'ajv/elfeed-kill-buffers nil 'make-it-local))
