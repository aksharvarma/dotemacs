(defvar ajv/my-init-directory nil
  "The directory which contains my init files")

(defvar ajv/symlink-folder nil
  "The folder which contains all the symlinks to other places in my filesystem.")

(defvar ajv/custom-file-name nil
  "The file which contains all the settings that Customize adds. This should be empty except for the list of packages installed. Everything is moved into a separate file according to its category.")

(defvar ajv/prefered-font-name nil
  "The preferred font.")

(defvar ajv/prefered-dark-theme-name nil
  "The preferred dark theme.")

(defvar ajv/prefered-light-theme-name nil
  "The preferred light theme.")

(setq ajv/my-init-directory (concat user-emacs-directory "site-lisp/ajv/")
      ajv/symlink-folder "~/0/"
      ajv/custom-file-name "ajv-customizations.el"
      ajv/prefered-font-name "dejavu sans mono 11"
      ajv/prefered-dark-theme-name 'deeper-blue
      ajv/prefered-light-theme-name 'tsdh-light)

(defvar ajv/yasnippets-directory nil
  "The folder which contains all my yasnippet snippets.")

(setq ajv/yasnippets-directory (concat user-emacs-directory "site-lisp/ajv/snippets"))

(defvar ajv/dired-default-sorting-alist nil
  "The default sorting of various directories for dired.")

(setq ajv/dired-default-sorting-alist
      '(("~/bin/" . "(X)")
	("~/Documents/" . "(X)")
        ("~/Downloads/" . "(t)")))

(defface ajv/display-time-face
  '((((type x w32 mac))
     ;; #060525 is the background colour of my default face.
     (:foreground "#99FF00" :background "#121212" :inherit bold))
    (((type tty))
     (:foreground "#99FF00" :background "#121212")))
  "Face used to display the time in the mode line. This particular face is used for better visibility")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variables defined in ajv-sensitive-settings.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar ajv/sensitive/my-org-agenda-self-file nil
  "The org agenda file for \"self\"
 Set in ajv-sensitive-settings.el")

(defvar ajv/sensitive/my-org-agenda-admin-file nil
  "The org agenda file for various administrative things.
 Set in ajv-sensitive-settings.el")

(defvar ajv/sensitive/my-org-agenda-work-file nil
  "The org agenda file for work related things.
 Set in ajv-sensitive-settings.el")

(defvar ajv/sensitive/my-org-agenda-research-file nil
  "The org agenda file for research projects
 Set in ajv-sensitive-settings.el")

(defvar ajv/sensitive/my-org-agenda-personal-file nil
  "The org agenda file for personal projects.
 Set in ajv-sensitive-settings.el")

(defvar ajv/sensitive/my-elfeed-org-file nil
  "The file from which my blog list is taken for elfeed
 Set in ajv-sensitive-settings.el")

(defvar ajv/sensitive/oeuvre-filename nil
  "The file that contains the oeuvre list.
 Set in ajv-sensitive-settings.el")

(defvar ajv/sensitive/my-birthdate nil
  "My birthdate in YYYY-MM-DD format for the memento-mori package.
 Set in ajv-sensitive-settings.el")

;; (defvar ajv/sensitive/my-quotes-filename nil
;;   "The file that contains the quotes that ajv/quotes uses.
;;  Set in ajv-sensitive-settings.el")

;;;;;;;;;;
;; defvars for ajv-sensitive-settings.el end here
;;;;;;;;;;

;;;;;;;;;;
;; Now to use them (after loading them.)
;;;;;;;;;;

(setq ajv/sensitive/my-org-agenda-files-dir user-emacs-directory)

(setq ajv/sensitive/my-elfeed-org-file "")
(setq ajv/sensitive/oeuvre-filename "")
(setq ajv/sensitive/my-quotes-filename "")
(setq ajv/sensitive/my-birthdate "1970-01-01")	;Defaults to this.


(setq ajv/my-sensitive-settings-file-name (concat user-emacs-directory "site-lisp/ajv/ajv-sensitive-settings.el"))
;; Load the sensitive settings if they exist
(if (file-exists-p ajv/my-sensitive-settings-file-name)
    (load ajv/my-sensitive-settings-file-name)
  nil)

(setq ajv/my-org-agenda-files (list ajv/sensitive/my-org-agenda-files-dir))

(setq ajv/my-elfeed-org-file-list (list ajv/sensitive/my-elfeed-org-file))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
