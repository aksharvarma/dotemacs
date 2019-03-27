(defvar ajv/my-init-directory nil
  "The directory which contains my init files")

(defvar ajv/symlink-folder nil
  "The folder which contains all the symlinks to other places in my filesystem.")

(defvar ajv/custom-file-name nil
  "The file which contains all the settings that Customize adds. This should be empty except for the list of packages installed. Everything is moved into a separate file according to its category.")

(defvar ajv/prefered-font-name nil
  "The preferred font.")

(defvar ajv/prefered-theme-name nil
  "The preferred theme.")

(setq ajv/my-init-directory "~/.emacs.d/site-lisp/ajv/"
      ajv/symlink-folder "~/0/"
      ajv/custom-file-name "ajv-customizations.el"
      ajv/prefered-font-name "dejavu sans mono 10"
      ajv/prefered-theme-name 'deeper-blue)

(defvar ajv/yasnippets-directory nil
  "The folder which contains all my yasnippet snippets.")

(setq ajv/yasnippets-directory "~/.emacs.d/site-lisp/ajv/snippets")

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

(defvar ajv/my-org-agenda-files nil
  "The list of files to consider when creating the Agenda view.
 Set in ajv-sensitive-settings.el")

(defvar ajv/my-elfeed-org-file nil
  "The file from which my blog list is taken for elfeed
 Set in ajv-sensitive-settings.el")

(defvar ajv/my-quotes-filename nil
  "The file that contains the quotes that ajv/quotes uses.
 Set in ajv-sensitive-settings.el")
