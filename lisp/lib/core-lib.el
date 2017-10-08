;; -*- lexical-binding : t byte-compile-dynamic : t -*-

(defun ignore-true (&rest _ignore)
  "This function accepts any number of arguments but ignore them 
and return t"
  t)

(provide 'core-lib)
;;; core-lib.el ends here
