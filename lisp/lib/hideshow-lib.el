;;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

(require 'util-lib)
(eval-when-compile
  (require 'silence-code))

(defun toggle-selective-display (column)
  "Toggle hide or show using selective display."
  (interactive "P")
  (set-selective-display
   (or column
       (unless selective-display
	 (1+ (current-column))))))

(defun toggle-hideshow-block (column)
  "Toggle hide or show block."
  (interactive "P")
  (code-silence
   (if (and (boundp 'hs-minor-mode) hs-minor-mode)
       (save-excursion
	 (hs-toggle-hiding))
     (toggle-selective-display column))))

(provide 'hideshow-lib)
;;; hideshow-lib.el ends here
