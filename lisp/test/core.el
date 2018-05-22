;;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

;;; Commentary:

;;; Code:

(/require-meta core)

(/message-test-start)
(defvar vara (make-symbol "unintern-symbol") "Test")
(defmacro testma ()
  "Test unintern-symbol."
  (set vara 100)
  (setq unintern-symbol 11111)
  (print (eval vara))
  `(let* ((,vara 111))
     (print ,vara)
     (setq ,vara 1000)
     (print ,vara)
     (let* ((,vara 222))
       (print ,vara))
     (print ,vara)))
; (testma) (print (eval vara))

;; (let* ((ll '(1 2 3)))
;;   (dolist (e (1 2 3))
;;     (print e)))

(defmacro testb ()
  "Test b."
  (/--sexp
    (/--sexp-append-1 'message)
    (/--sexp-append-1 "fuck you %s %s")
    (/--sexp-exec "c" "d")))

(defmacro testc ()
  "Test c."
  (/--sexp-progn-exec
    (message "a") (message "b") (message "c")))

(defmacro testd ()
  "Test d."
  (/--sexp-cond-case
    ((boundp 'a) (message "bound a"))
    (t (message "a") (message "b"))))

(/ppmacroexpand-all (/--sexp-append a b c))
(/ppmacroexpand-all (/--sexp-progn (/--sexp-append-1 a)))
(/ppmacroexpand-all (testd))
(setq a 100)
(testd)
(/message-test-end)

(/provide)
;;; test/core.el ends here
