(provide 'ajv-git-gutter-fringe)

(fringe-helper-define 'git-gutter-fr:modified nil
  "..XXXX.."
  "XX....XX"
  "XX....XX"
  "XX....XX"
  "XX....XX"
  "XX....XX"
  "XX....XX"
  "..XXXX..")

(set-face-foreground 'git-gutter-fr:modified "yellow")
(set-face-foreground 'git-gutter-fr:added    "green")
(set-face-foreground 'git-gutter-fr:deleted  "red")

(setq git-gutter-fr:side 'right-fringe)
(setq git-gutter:update-interval 1)
