;;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

;;; Commentary:

;;; Code:

(/require-meta core)

(print (/regexp-quote "(abcd)"))
(print (/regexp-quote "`..?'"))

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

;; (setq lst nil)
;; (print lst)
;; (dolist (e '(1 2 3))
;;   (push `(aaa ,e) lst))
;; (print lst)

(defmacro testb ()
  "Test b."
  (/--sexp
    (/--sexp-append-1 'message)
    (/--sexp-append-1 "fuck you %s %s")
    (/--sexp-exec "c" "d")))

(defmacro testc ()
  "Test c."
  (/--sexp-progn
    ; (/--sexp-append-1 '(message "a%s" 1234))
    (/--sexp-append
      '(message "b") '(message "c"))))

(defmacro testd ()
  "Test d."
  (/--sexp-cond-case
    ((boundp 'a) (message "bound a"))
    (t (message "a") (message "b"))))

(defmacro teste (&rest body)
  "Test e."
  `(let ((,/--sexp-list nil))
     (dolist (e ,body) (push e ,/--sexp-list))
     ))

;(/ppmacroexpand-all (/--sexp-append a b c))
;(/ppmacroexpand-all (/--sexp-progn (/--sexp-append a)))
(/ppmacroexpand-all (testc))
(setq a 100)
(testc)
(/message-test-end)

(/provide)
;;; test/core.el ends here
