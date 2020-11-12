;;; ajv-scpaste.el --- Paste to server via TRAMP preserving Emacs' font-lock syntax and highlighting.

;; The idea is heavily influenced by: https://www.emacswiki.org/emacs/SCPaste
;; The code for that is available here: https://p.hagelb.org/
;; My implementation uses htmlfontify which is inbuilt.
;; Additionally, it uses TRAMP, which should make it more robust, I think.
;;

(provide 'ajv-scpaste)

(defvar ajv/scpaste/tramp-full-foldername
  (concat ajv/sensitive/website-tramp-foldername "files/pastes/")
  "The full path of the folder in which to put these pastes.")

(defun ajv/scpaste/add-footer ()
  "HTML message to place at the bottom of each file."
  (goto-char (point-max))
  (search-backward "</body>\n</html>")
  (insert (concat "<hr/><p style='float:right; vertical-align:text-bottom; font-size: 8pt; font-family: monospace; "
		  (mapconcat (lambda (c) (concat c "-select: none"))
			     '("-moz-user" "-webkit-user" "-ms-user" "user") "; ")
		  "'>Generated using HTML Fontify in Emacs at "
		  (current-time-string) " " (cadr (current-time-zone)) ".\n")))

(defun ajv/scpaste/add-mobile-friendly-tag ()
  "Add meta HTML tags in the <head> to try and make this mobile friendly"
  (goto-char (point-min))
  (search-forward "<head>")
  (insert "\n<meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0, user-scalable=yes\" />"))

(defun ajv/scpaste/paste-region-or-buffer (filename contents)
  "Write a CSS+HTML file to ajv/scpaste/tramp-full-foldername.
This uses the current font-lock and syntax highlighting of the input.
If there is a region selected, use that otherwise ask for a buffer to use."
  (interactive
   (let* (;; DWIM logic: Region if exists, otherwise ask for buffer
	  ;; If there is already a region selected, use that
	  ;; Otherwise, ask for a buffer and use its point-min and point-max
	  ;; In both cases, contents will have the content and the buffer-name
	  ;; The buffer-name part is only for helping with filename finding.
	  ;; Note the use of buffer-substring vs. buffer-substring-no-properties
	  ;; This ensures that the font-lock and other Emacs syntax highlighting
	  (contents (if (region-active-p) ; If the region is active
			(cons (buffer-substring (region-beginning) (region-end))
			      (buffer-name))
		      (with-current-buffer ; Otherwise ask for a buffer
			  (read-buffer "Buffer: " (current-buffer) t)
			(cons (buffer-substring (point-min) (point-max))
			      (buffer-name)))))
	  ;; What filename should be used finally?
	  ;; Initial suggestion is based on the buffer
	  ;; from which contents were taken
	  (filename (completing-read "Filename: " nil nil nil
				     (concat (cdr contents) ".html") nil
				     (concat (cdr contents) ".html") nil)))
     ;; We drop the buffer-name since we won't need it anymore
     (list filename (car contents))
     ))
  ;;In a new buffer:
  (with-temp-buffer
    ;; Add the contents we want
    (insert contents)
    ;; rename buffer so that we will end up using the accurate title
    (rename-buffer filename)
    ;;do the magic
    (htmlfontify-buffer)

    ;; Add a tag to make the viewing mobile friendly
    (ajv/scpaste/add-mobile-friendly-tag)
    ;; Insert a footer mentioning when and how this was made.
    (ajv/scpaste/add-footer)
    ;; Finally write the file
    (write-file (concat ajv/scpaste/tramp-full-foldername filename) t)
    ;; Kill the HTML buffer once done.
    (kill-buffer)))
