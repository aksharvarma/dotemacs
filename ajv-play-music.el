(provide 'ajv-play-music)

(defvar ajv/music-buffer-name nil
  "Name given to the buffer that will call mpv and play music.
There will only be one (we assume that you'll only listen to one piece of music at a time.)")

(setq ajv/music-buffer-name "ajv-mpv-buffer")

(defvar ajv/music-buffer-created-flag nil
  "Flag to figure out if music-buffer-created or not.")

(setq ajv/music-buffer-created-flag nil)

(defun ajv/check-if-created-music-buffer ()
  "Start using this instead of a flag variable."
  (interactive)
  (if (get-buffer (concat "*" ajv/music-buffer-name "*")) t nil))

(defun ajv/create-music-buffer ()
  "Create an ansi-term buffer with the name `ajv/music-buffer-name`"
  (interactive)
  (ansi-term "/bin/bash" ajv/music-buffer-name)
  (execute-kbd-macro (read-kbd-macro "C-x z"))
  (setq ajv/music-buffer-created-flag t))

(defun ajv/kill-music-buffer ()
  "Kill the buffer that was opened to play music."
  (interactive)
  (if (ajv/check-if-created-music-buffer)
      (progn
	(execute-kbd-macro (read-kbd-macro (concat "s-b " ajv/music-buffer-name
						   " RET s-w yes RET")))
	(setq ajv/music-buffer-created-flag nil))
    (message "Music buffer not created. Nothing to do."))
  )

(defun ajv/stop-music ()
  "Stop the music that is playing in ajv/music-buffer."
  (interactive)
  (if (ajv/check-if-created-music-buffer)
      (execute-kbd-macro (read-kbd-macro (concat "s-b " ajv/music-buffer-name
						 " RET C-c C-c C-x z")))
    (message "Music buffer not created. Nothing to do."))
  )


(defun ajv/play-pause-music ()
  "Toggle function to play/pause music playing in `ajv/music-buffer-name`"
  (interactive)
  (if (not (ajv/check-if-created-music-buffer))
      (ajv/play-this-music)
    (execute-kbd-macro (read-kbd-macro (concat "s-b " ajv/music-buffer-name " RET SPC C-x z"))))
  )

(defun ajv/play-next-music ()
  "Toggle function to play/pause music playing in `ajv/music-buffer-name`"
  (interactive)
  (if (not (ajv/check-if-created-music-buffer))
      (ajv/play-this-music)
    (execute-kbd-macro (read-kbd-macro (concat "s-b " ajv/music-buffer-name " RET > C-x z"))))
  )

(defun ajv/play-previous-music ()
  "Toggle function to play/pause music playing in `ajv/music-buffer-name`"
  (interactive)
  (if (not (ajv/check-if-created-music-buffer))
      (ajv/play-this-music)
    (execute-kbd-macro (read-kbd-macro (concat "s-b " ajv/music-buffer-name " RET < C-x z"))))
  )

(defun ajv/modify-filename-kbd-macro-syntax (filename)
  "Modify the filename so that we can simply concat it in a read-kbd-macro call.
This requires adhering to the syntax that edmacro uses, i.e. handling spaces using SPC and so on."
  (interactive)
  (concat "\"" (replace-regexp-in-string " " " SPC " filename  t t) "\"")
  )

(defun ajv/go-to-folder-in-music-buffer (foldername)
  "Go to the given folder in ajv/music-buffer"
  (interactive)
  (if (not (ajv/check-if-created-music-buffer)) (ajv/play-this-music) nil)
  (execute-kbd-macro (read-kbd-macro (concat "s-b " ajv/music-buffer-name " RET cd SPC " (ajv/modify-filename-kbd-macro-syntax foldername) " RET")))
  )

(defun ajv/play-music-from-given-folder (foldername)
  "Play music given a foldername as argument"
  (interactive)
  (if (not (ajv/check-if-created-music-buffer)) (ajv/play-this-music) nil)
  (ajv/go-to-folder-in-music-buffer foldername)
  (execute-kbd-macro (read-kbd-macro "mpv SPC --force-window=no SPC --player-operation-mode=cplayer SPC --shuffle SPC --loop-playlist SPC --quiet SPC * RET C-x z"))
  )

(defun ajv/play-this-music ()
  "Search for a folder (using ido) and then play music from that folder."
  (interactive)
  (if (not (ajv/check-if-created-music-buffer)) (ajv/create-music-buffer) nil)
  (with-current-buffer (let ((default-directory (file-truename "~/0/music/")))
			 (ido-dired))
    (beginning-of-buffer)
    (setq chosen-foldername (dired-copy-filename-as-kill))
    (ajv/kill-this-buffer))
  (ajv/stop-music)
  (ajv/play-music-from-given-folder chosen-foldername)
  )
