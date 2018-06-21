(setq ajv/my-init-directory "~/.emacs.d/site-lisp/ajv/"
      ajv/symlink-folder "~/0/"
      ajv/custom-file-name "ajv-customizations.el"
      ajv/prefered-font-name "dejavu sans mono 10"
      ajv/prefered-theme-name 'deeper-blue)

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
