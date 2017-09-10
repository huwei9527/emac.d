;;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

(require 'util-lib)

(defun toggle-selective-display (column)
  "Toggle hide or show using selective display."
  (interactive "P")
  (with-no-message
   (set-selective-display
    (or column
	(unless selective-display
	  (1+ (current-column)))))))

(defun toggle-hideshow-block (column)
  "Toggle hide or show block."
  (interactive "P")
  (if (and (boundp hs-minor-mode) hs-minor-mode)
      (save-excursion
	(with-no-message
	 (hs-toggle-hiding)))
    (toggle-selective-display column)))



(provide 'hideshow-lib)
;;; hideshow-lib.el ends here
