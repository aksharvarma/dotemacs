;;; dired-sorting.el 
;; This is my version of dired-sort-map.
;; Inspired by dired-sort-map.el by Patrick Anderson, et al.



;;;>>>
;;;All this just to get Downloads to start sorted by time.
;;;These things work in dired, sorting the listings...
;;;The t is for time.
;;;The n is for name.
;;;The X is for eXtension.
;;;The S is for Size.
;; That was the original reason why I started working on this.
;; I wanted to have my Downloads folder to be always sorted by time.


(provide 'ajv-dired-sorting)
;;;This was copied directly from there.
(require 'dired)
(defvar dired-sort-map (make-sparse-keymap))
;;Copied ends.

(defun sort-by-time ()
  "sort by Time"
  (interactive)
  (dired-sort-other (concat dired-listing-switches " -t")))

(defun sort-by-name ()
  "sort by name"
  (interactive)
  (dired-sort-other (concat dired-listing-switches " -n")))

(defun sort-by-eXtension ()
  "sort by eXtension"
  (interactive)
  (dired-sort-other (concat dired-listing-switches " -X")))

(defun sort-by-Size ()
  "sort by Size"
  (interactive)
  (dired-sort-other (concat dired-listing-switches " -S")))

;; (defun sort-in-reverse ()
;;   "sort in reverse"
;;   (interactive)
;;   (if (string-match "-r" dired-listing-switches)
;;       (dired-sort-other
;;        (setq dired-listing-switches 
;; 	     (replace-match "" nil nil dired-listing-switches)))
;;     (dired-sort-other 
;;      (setq dired-listing-switches (concat dired-listing-switches " -r")))))
  
;; (defun sort-in-reverse ()
;;   "sorts in reverse order of what was the sort"
;;   (interactive)
;;   (dired-sort-other 
;;    (setq temp-str
;; 	 (if (string-match "-r" dired-listing-switches)
;; 	     (replace-match "" nil nil dired-listing-switches)
;; 	   (concat dired-listing-switches " -r"))))
;; (string (replace-regexp-in-string " -r" "" dired-listing-switches))

;; (setq dired-listing-switches (replace-regexp-in-string "  " "" dired-listing-switches))


;; (setq dired-listing-switches (concat dired-listing-switches " -r"))
;; dired-listing-switches
;; (equal (string-match "-r" dired-listing-switches) nil)
;; (setq str "-alLh --group-directories-first -r")
;; (string-match "-r"  dired-listing-switches)
;; (setq dired-listing-switches (replace-match "" nil nil dired-listing-switches))

 
;; (match-end 0)

;;The following line is a working undo for adding -r.
;;(nth 1 (split-string (format "%s" (split-string dired-listing-switches "-r")) "[()]"))

;;;Stupid trial and errors. Errors in this case.
;; (combine-and-quote-strings (split-string "-alLh --group-directories-first -r" "-r") " ")
;; (concatenate 'string (split-string "-alLh --group-directories-first -r" "-r"))
;; (format "%s" (split-string "-alLh --group-directories-first -r" "-r"))
;; (concat (split-string "-alLh --group-directories-first -r" "-r"))

;;;Calling above guy only when dired mode and when the buffer
;;; is in the list.
;;;Adding to the list in the sort-listed-buffer-by-time 
;;;funtion, will make sure that those things start sorted
;;;according to time too.
;;;Same for the other sort-listed-buffer-... functions too.
;;;Use relative pathnames. Can't find anything that works with
;;;Absolute pathnames. I am currently using the default-directory
;;;variable which stores the relative pathname.
;;;Also make sure that you trail it with a /


;;;This list is for time sorting.
(setq buffers-to-sort-by-time ;Enter relative pathnames.
      '("~/Downloads/"))

(defun sort-listed-buffers-by-time ()
  "Files in the list will sort by time on startup"
  (interactive)
  (if (member default-directory buffers-to-sort-by-time)
      (sort-by-time)
    nil))


;;;This list is for name sorting.
(setq buffers-to-sort-by-name ;Enter relative pathnames.
      '(""))

(defun sort-listed-buffers-by-name ()
  "Files in the list will be sorted by name on startup"
  (interactive)
  (if (member default-directory buffers-to-sort-by-name)
      (sort-by-name)
    nil))

;;;This list is for eXtension sorting.
(setq buffers-to-sort-by-eXtension ;Enter relative pathnames.
      '("~/Documents/" "~/bin"))

(defun sort-listed-buffers-by-eXtension ()
  "Files in the list will be sorted by eXtension on startup"
  (interactive)
  (if (member default-directory buffers-to-sort-by-eXtension)
      (sort-by-eXtension)
    nil))

;;;This list is for Size sorting.
(setq buffers-to-sort-by-Size ;Enter relative pathnames.
      '(""))

(defun sort-listed-buffers-by-Size ()
  "Files in the list will be sorted by Size on startup"
  (interactive)
  (if (member default-directory buffers-to-sort-by-Size)
      (sort-by-Size)
    nil))

;;;Adding the hook to make these work as soon as dired opens.
(add-hook 'dired-mode-hook 'sort-listed-buffers-by-time)
(add-hook 'dired-mode-hook 'sort-listed-buffers-by-name)
(add-hook 'dired-mode-hook 'sort-listed-buffers-by-eXtension)
(add-hook 'dired-mode-hook 'sort-listed-buffers-by-Size)

;;;}}}

