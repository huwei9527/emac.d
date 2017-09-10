;; -*- lexical-binding : t byte-compile-dynamic : t -*-

(defun super-tab ()
  "Tab to do everything."
  (interactive)
  (unless (call-interactively 'company-manual-begin)
    (call-interactively 'indent-for-tab-command))
  )


(provide 'miscellany-lib)
; miscellany.el ends here
