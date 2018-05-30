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

(defvar /--write-region nil
  "Store the symbol-function of original `write-region'.")
(fset '/--write-region (symbol-function 'write-region))
(defun /write-region-silently (start end filename &optional append visit
				     lockname mustbenew)
  "`write-region' with \"Wrote file\" message suppressed.
See `/--write-resion'. The original is `write-region' but we replace it."
  (/--write-region start end filename append 'nomsg lockname mustbenew))

;(fset 'write-region (symbol-function '/write-region-silently))

(/provide)
;;; lib/miscellany.el ends here
