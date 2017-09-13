;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

(require 'util-lib)

(defun tf (el pos path)
  ""
  (if (eq el 'c)
      (progn
	(setf (elt (car path) pos) 'fuck)
	nil)
    t))

(defun test (a)
  ""
  (message "%s" a)
  (sequence-element-filter a 'tf)
  (message "%s" a))

(test '(a (b) [c (d e) f] g))
(test '(c (b c) [d (c [b c b] d) a] c))

(provide 'sequence-test)
;;; sequence-test.el ends here
