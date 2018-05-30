;;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

;;; Commentary:

;;; Code:

(/require-lib core hideshow)

(defun /tab-dwim (prefix)
  "Tab everything."
  (interactive "P")
  (if prefix
      (if (numberp prefix)
	  (insert-char ?\t prefix)
	(insert-char ?\t 1))
    (let* ((old (point)))
      (unless (and (/char-at-point-word-p)
		   (boundp 'company-mode)
		   company-mode
		   (company-manual-begin))
	(call-interactively 'indent-for-tab-command)
	(and (eq old (point))
	     ;(fboundp 'toggle-hideshow-block)
	     (call-interactively '/toggle-hideshow-block))))))

(/provide)
;;; lib/miscellany.el ends here
