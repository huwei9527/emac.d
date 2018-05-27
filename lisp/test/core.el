;;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

;;; Commentary:

;;; Code:

(/require-meta core)
(/require-lib core)


(/message-test-start)

(when nil
  (print (/regexp-quote "(abcd)"))
  (print (/regexp-quote "`..?'")))

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
    '((boundp 'a) (message "bound a"))
    '(t (message "a") (message "b"))))

(defmacro teste (&rest body)
  "Test e."
  `(let ((,/--sexp-list nil))
     (dolist (e ,body) (push e ,/--sexp-list))
     ))

;(/ppmacroexpand-all (/--sexp-append a b c))
;(/ppmacroexpand-all (/--sexp-progn (/--sexp-append a)))
; (/ppmacroexpand-all (testc))
(setq a 100)
; (testc)

(defvar aaa 1)
;(message "before aaa: %s" aaa)

(eval-after-load '"~/.emacs.d/test_compile"
  '(testd))

(eval-after-load "~/.emacs.d/test_compile"
  '(setq aaa 2))
; (message "eval aaa: %s" aaa)

; (load "/home/hw/Projects/emacs.d/test_compile.el")
; (message "load aaa: %s" aaa)

;(/ppmacroexpand-all (/eval-after-load abcd a b c d))
;(/eval-after-load abcd a b c d)

(when nil
  (message "%s" (/--list-quote-all '(a b c d)))
  (message "%s" (/--list-quote-odd '(a b c d)))
  (message "%s" (/--list-quote-even '(a b c d))))

; (/ppmacroexpand-all (/--sexp (/--sexp-append-1-literally a) (/--sexp-append-1-literally b)))
; (/--sexp (/--sexp-append-1-literally a) (/--sexp-append-1-literally b))
; (/ppmacroexpand-all (/--sexp (/--sexp-append-literally 1 2 3)))
; (print (/--sexp (/--sexp-append-literally a b c d e f g)))
;; (/ppmacroexpand-all (/--sexp (/--sexp-append-literally-odd 1 2 3 4)))
;; (print (/--sexp (/--sexp-append-literally-odd a 'b c 'd)))

(when nil
  (defmacro teste (a c b d)
    "Test e"
    (/--sexp-setq
      (/--sexp-pair 'a c 'b d)))
  (print (/--sexp-setq (/--sexp-pair 'a (+ 123 100) 'b (+ 456 100))))
  (teste a (1+  123) b (1- 456))
  (message "a = %s b = %s" a b))

(when nil
  (message "even: %s %s" (/evenp 1) (/evenp 2))
  (message "odd : %s %s" (/oddp 101) (/oddp 102))
  (let* ((tmp 1) (var 2))
    (message "before: tmp %s var %s" tmp var)
    (/swap tmp var)
    (message "after: tmp %s var %s" tmp var)))

(when t
  (let* ((cnt 0))
    (when nil
      (/test-char "a" /--character)
      (/test-char ?a /--character)
      (/test-char ab /--character)
      (/test-char "ab" /--character)
      )
    (when nil
      (/test-char ?\\ /char-escape-p)
      (/test-char / /char-escape-p)
      (/test-char \\ /char-escape-p)
      (/test-char "\\" /char-escape-p)
      (/test-char "\\a" /char-escape-p)
      )
    (when nil
      (/test-char " " /char-space-p)
      (/test-char ?- /char-space-p)
      (/test-char \  /char-space-p)
      (/test-char ?\t /char-space-p)
      (/test-char ?\r /char-space-p)
      (/test-char ?\n /char-space-p)
      (/test-char a /char-space-p)
      )
    (when nil
      (/test-char a /char-word-p)
      (/test-char A /char-word-p)
      (/test-char 1 /char-word-p)
      (/test-char ?_ /char-word-p)
      (/test-char ?1 /char-word-p)
      (/test-char ?9 /char-word-p)
      )
    (when t
      (/test-char a /char-symbol-p)
      (/test-char ?1 /char-symbol-p)
      (/test-char _ /char-symbol-p)
      (/test-char - /char-symbol-p)
      (/test-char + /char-symbol-p)
      (/test-char * /char-symbol-p)
      (/test-char / /char-symbol-p)
      )
    ))


(/message-test-end)

(/provide)
;;; test/core.el ends here
