;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

(defun company-newline ()
  "Abort company-complete and insert a new line."
  (interactive)
  (company-abort)
  (newline))


(provide 'company-lib)
;;; company-lib.el ends here
