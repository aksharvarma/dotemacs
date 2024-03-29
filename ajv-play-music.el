(provide 'ajv-play-music)

(defvar ajv/music/buffer-name nil
  "Name given to the buffer that will call mpv and play music.
There will only be one (we assume that you'll only listen to one piece of music at a time.)")

(setq ajv/music/buffer-name "ajv-mpv-buffer")

(defvar ajv/music/buffer-name-with-asterisks nil
  "ajv/music/buffer-name with asterisks concatenated in at both ends for use in switch-to-buffer")

(setq ajv/music/buffer-name-with-asterisks (concat "*" ajv/music/buffer-name "*"))

(defun ajv/music/check-if-stopped ()
  "Start using this instead of a flag variable."
  (interactive)
  (with-current-buffer ajv/music/buffer-name-with-asterisks
    (string-equal (file-truename "~/") default-directory)))

(defun ajv/music/check-if-created-buffer ()
  "Start using this instead of a flag variable."
  (interactive)
  (if (get-buffer ajv/music/buffer-name-with-asterisks) t nil))

(defun ajv/music/keybinding-string-for-command (command-name)
  "Find the string representation of the keybinding that command uses"
  (replace-regexp-in-string
   "\\(.*?is on \\)\\(.*?\\)\\(,.*$\\|$\\)"
   "\\2"
   (message (with-output-to-string (where-is command-name)))))

(defun ajv/music/create-buffer ()
  "Create an ansi-term buffer with the name `ajv/music/buffer-name`"
  (interactive)
  (ansi-term "/bin/bash" ajv/music/buffer-name)
  (bury-buffer))

(defun ajv/music/kill-buffer ()
  "Kill the buffer that was opened to play music."
  (interactive)
  (if (ajv/music/check-if-created-buffer)
      (progn
	(switch-to-buffer ajv/music/buffer-name-with-asterisks)
	(ajv/kill-this-buffer)
	(execute-kbd-macro
	 (read-kbd-macro
	  " yes RET")))
    (message "Music buffer not created. Nothing to do.")))

(defun ajv/music/stop-playing ()
  "Stop the music that is playing in ajv/music/buffer-name."
  (interactive)
  (if (ajv/music/check-if-created-buffer)
      (progn
	(switch-to-buffer ajv/music/buffer-name-with-asterisks)
	(execute-kbd-macro (read-kbd-macro " C-c C-c cd RET"))
	(bury-buffer))
    (message "Music buffer not created. Nothing to do.")))

(defun ajv/music/send-mpv-command (str)
  "Send str command to the mpv process"
  (interactive)
  (if (or (not (ajv/music/check-if-created-buffer))
	  (ajv/music/check-if-stopped))
      (ajv/music/play-this)
    (save-excursion
      (switch-to-buffer (concat "*" ajv/music/buffer-name "*"))
      (execute-kbd-macro (read-kbd-macro str))
      (bury-buffer)
      (message (concat "Sent" str "to mpv")))))

(defun ajv/music/switch-to-mine ()
  (interactive)
  (defun ajv/music/play-pause ()
    "Toggle function to play/pause music playing in `ajv/music/buffer-name`"
    (interactive)
    (ajv/music/send-mpv-command " SPC "))

  (defun ajv/music/play-next ()
    "Toggle function to play/pause music playing in `ajv/music/buffer-name`"
    (interactive)
    (ajv/music/send-mpv-command " > "))

  (defun ajv/music/play-previous ()
    "Toggle function to play/pause music playing in `ajv/music/buffer-name`"
    (interactive)
    (ajv/music/send-mpv-command " < "))
  )

(defun ajv/music/switch-to-emms ()
  (interactive)
  (defun ajv/music/play-pause ()
    "Toggle function to play/pause music playing in `ajv/music/buffer-name`"
    (interactive)
    (emms-pause))

  (defun ajv/music/play-next ()
    "Toggle function to play/pause music playing in `ajv/music/buffer-name`"
    (interactive)
    (emms-next))

  (defun ajv/music/play-previous ()
    "Toggle function to play/pause music playing in `ajv/music/buffer-name`"
    (interactive)
    (emms-previous))
  )


(defun ajv/music/play-song-in-loop ()
  "Toggle function to play/pause music playing in `ajv/music/buffer-name`"
  (interactive)
  (ajv/music/send-mpv-command " L "))

(defun ajv/music/modify-filename-kbd-macro-syntax (filename)
  "Modify the filename so that we can simply concat it in a read-kbd-macro call.
This requires adhering to the syntax that edmacro uses, i.e. handling spaces using SPC and so on."
  (interactive)
  (concat "\"" (replace-regexp-in-string " " " SPC " filename  t t) "\""))

(defun ajv/music/go-to-folder-in-music-buffer (foldername)
  "Go to the given folder in ajv/music/buffer-name"
  (interactive)
  (unless (ajv/music/check-if-created-buffer) (ajv/music/play-this))
  (progn
    (switch-to-buffer (concat "*" ajv/music/buffer-name "*"))
    (execute-kbd-macro
     (read-kbd-macro
      (concat " cd SPC " (ajv/music/modify-filename-kbd-macro-syntax foldername) " RET")))))

(defun ajv/music/play-from-given-folder (foldername shuffle-flag)
  "Play music given a foldername as argument"
  (interactive)
  (unless (ajv/music/check-if-created-buffer) (ajv/music/play-this))
  (ajv/music/go-to-folder-in-music-buffer foldername)
  (execute-kbd-macro
   (read-kbd-macro
    (concat "mpv SPC --force-window=no SPC " shuffle-flag " SPC --player-operation-mode=cplayer SPC --no-audio-display SPC --loop-playlist SPC --quiet SPC * RET ")))
  (bury-buffer))

(defun ajv/music/play-this ()
  "Search for a folder (using ido) and then play music from that folder."
  (interactive)
  (unless (ajv/music/check-if-created-buffer) (ajv/music/create-buffer))
  (with-current-buffer (helm-find-files-1 (file-truename "~/0/music/"))
    ;; (let ((default-directory (file-truename "~/0/music/")))
    ;; 	(ido-dired))
    ;; (goto-char (point-min))
    (setq chosen-foldername (ajv/dired/copy-directory-name-as-kill))
    (ajv/kill-this-buffer)
    (setq shuffle-flag (completing-read "Set shuffle flag:" '(" " "--shuffle"))))
  (ajv/music/stop-playing)
  (ajv/music/play-from-given-folder chosen-foldername shuffle-flag))
