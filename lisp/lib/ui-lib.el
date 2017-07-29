;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

(let ((dflt-fg (face-foreground 'mode-line))
      (dflt-bg (face-background 'mode-line)))
  (defun set-mode-line-color (&optional fg bg)
    "Set the foreground FG and background BG color for modeline.

If fg is nil, restore the default color."
    (unless fg
      (setq fg dflt-fg bg dflt-bg))
    (set-face-foreground 'mode-line fg)
    (set-face-background 'mode-line bg)))

(provide 'ui-lib)
; ui-lib ends here