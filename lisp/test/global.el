;;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

;;; Commentary:

;;; Code:

(/message-test-start)
; (pp-macroexpand-expression '(/require-init abc))
; (pp-macroexpand-expression '(/def-lisp-directory abc "Doc aaa"))
(setq aa 123 bb 'aa)
(let* ((a 123)
       (b 'a))
  (when nil
    (message "number: %s" (/--value 123))
    (message "vector: %s" (/--value [1 2 3]))
    (message "string: %s" (/--value "123"))
    (message "symub : %s" (/--value 'abc))
    (message "symlb : %s" (/--value 'a))
    (message "symlb : %s" (/--value b))
    (message "symgb : %s" (/--value 'aa))
    (message "symgb : %s" (/--value bb))
    )
  (when nil
    (message "number: %s" (/--name 123))
    (message "string: %s" (/--name "123"))
    (message "vector: %s" (/--name [1 2 3]))
    (message "symub : %s" (/--name 'abc))
    (message "symgb : %s" (/--name 'aa))
    ))
(when nil
  (when nil
    (let* ((path (expand-file-name "~/Projects/test/test_compile")))
      ;(byte-compile-file (format "%s.el" path))
      (require 'test_compile path)
      (message "featurep: %s" (featurep 'test_compile))
      ))
  (/--require 'test 'elpa)
  (message "exis: %s" (featurep '/test/elpa))
  ;(/--provide)
  )

(/message-test-end)

(/provide)

;;; test/global.el ends here
