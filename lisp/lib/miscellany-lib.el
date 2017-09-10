;; -*- lexical-binding : t byte-compile-dynamic : t -*-

(require 'util-lib)

(defun super-tab ()
  "Tab to do everything."
  (interactive)
  (unless (and (char-at-point-word-p)
	       (and (boundp 'company-mode) company-mode
		    (call-interactively 'company-manual-begin)))
    (call-interactively 'indent-for-tab-command)))


(provide 'miscellany-lib)
; miscellany.el ends here
