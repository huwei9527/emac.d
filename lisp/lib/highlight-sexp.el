;;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

;;; Commentary:

;;; Code:

(eval-when-compile (/require-meta hook))

(/require-custom highlight-sexp)
(require 'thingatpt)

(defvar /--highlight-sexp-overlay nil
  "The overlay for highlight-sexp mode to show sexp.")

(defun /highlight-sexp-highlight ()
  "Active the highlight-sexp overlay on the current sexp in the current window."
  (when /highlight-sexp-mode
    (unless (window-minibuffer-p (selected-window))
      (unless /--highlight-sexp-overlay
	(setq /--highlight-sexp-overlay (make-overlay 1 1))
	(overlay-put /--highlight-sexp-overlay 'face '/highlight-sexp))
      (overlay-put /--highlight-sexp-overlay 'window (selected-window))
      (save-excursion
	(condition-case nil
	    (backward-up-list 1)
	  (error nil))
	(let ((bounds (bounds-of-thing-at-point 'sexp)))
	  (when bounds
	    (move-overlay /--highlight-sexp-overlay
			  (car bounds) (cdr bounds) (current-buffer))))))))

(defun /highlight-sexp-unhighlight ()
  "Deactivate the highlight-sexp overlay on the current sexp in the current window."
  (and /--highlight-sexp-overlay (delete-overlay /--highlight-sexp-overlay)))

;;;###autoload
(define-minor-mode /highlight-sexp-mode
  "Hight light the sexp at point in the current window."
  :init-value nil
  :lighter nil
  :keymap nil
  :global nil
  (if /highlight-sexp-mode
      (progn
	(/add-hook (pre-command-hook) /highlight-sexp-unhighlight)
	(/add-hook (post-command-hook) /highlight-sexp-highlight))
    (/highlight-sexp-unhighlight)
    (/remove-hook (pre-command-hook) /highlight-sexp-unhighlight)
    (/remove-hook (post-command-hook) /highlight-sexp-highlight)))

(define-global-minor-mode /global-highlight-sexp-mode /highlight-sexp-mode
  /highlight-sexp-mode)

(/provide)
;;; lib/highlight-sexp.el ends here
