;; -*- lexical-binding : t byte-compile-dynamic : t -*-

(require 'util-lib)

(defun super-tab ()
  "Tab to do everything."
  (interactive)
  (let* ((point-last (point)))
    (unless (and (char-at-point-word-p)
		 (and (boundp 'company-mode) company-mode
		      (with-no-message
		       (call-interactively 'company-manual-begin))))
      (call-interactively 'indent-for-tab-command)
      (when (eq (point) point-last)
	(call-interactively 'toggle-hideshow-block)))))


(provide 'miscellany-lib)
; miscellany.el ends here
