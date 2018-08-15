;;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

;;; Commentary:

;;; Code:

(/message-test-start)

(let* ((a 123) (b 'a))
  (when nil
    (message "number: %s" (/--name 123))
    (message "string: %s" (/--name "123"))
    ;(message "vector: %s" (/--name [1 2 3]))
    (message "symub : %s" (/--name 'abc))
    (message "symgb : %s" (/--name 'aa))
    (message "list  : %s" (/--name '(custom "%s%s" ab cd)))
    ))

(when nil
  (message "%s" (/ppmacroexpand (/require-init a b c d))))

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
