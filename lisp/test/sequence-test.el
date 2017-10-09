;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

(require 'core-lib)

(defun tf (el)
  ""
  (if (eq el 'j) (list 100) nil))

(defun test (a)
  ""
  (message "%s" a)
  (sequence-filter a 'tf)
  (message "%s" a))

(test '(a (b) a [c a (d e) f] g (i . j)))
(test '(c (b c) [d (i . j) (c [b c b] d) a] c))

(provide 'sequence-test)
;;; sequence-test.el ends here
