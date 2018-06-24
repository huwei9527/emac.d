;;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

;;; Commentary:

;;; Code:

(/require-meta core)
(/require-lib core)

(/message-test-start)

(when nil
  (require 'macroexp)
  (defconst aaaa 1234)
  (message "%s" (macroexp--const-symbol-p 'aaaa))
  (message "quote   : %s" (macroexp-const-p ''aaaa))
  (message "keyword : %s" (macroexp-const-p :keyword))
  (message "lambda  : %s" (macroexp-const-p (lambda () nil)))
  (message "vector  : %s" (macroexp-const-p [1 2 3 4]))
  (message "char    : %s" (macroexp-const-p ?\t)))

(when nil
  (require 'cl-lib)
  (message "lambda  : %s" (cl--const-expr-p (lambda () nil)))
  (message "quote   : %s" (cl--const-expr-p ''aaaa)))

(when nil
  (/--def defun args &optional doc decl &rest body)
  ;(/--def "defun" args &optional doc &rest body)
  (/defun* tn () (declare (indent defun))
    (:documentation "doc1234")
    (message "%s" 123) (message "%s" 456))
  (/--tn)
  ;(/ppmacroexpand (/--def defconst init &optional doc))
  )

(when t
  (/defconst* test1-format (/--format-class :prefix "test1"))
  (/defconst* test2-format (/--format-class :suffix "test2"))
  (/defconst* test3-format (/--format-class :prefix "test3p" :suffix "test3s"))
  (message "%s" (/--format-internal /--custom-format "%s%s" 'a 'b))
  (message "%s" (/--format-internal nil "%s%s" 'a 'b))
  (message "%s" (/--format-internal `(,/--test1-format
				  ,/--test2-format
				  ,/--test3-format)
				"%s%s" 'a 'b))
  ;(message "%s" (/--format-base [1 2 3 4] "%s" 'a))
  (message "%s" (/--fmt /--custom-format "%s%s" 'a 'b))
  (message "%s" (/--fmt* /--test1-format "%s%s" 'a 'b))
  (message "%s" (/--int /--test2-format "%s%s" 'a 'b))
  (message "%s" (/--int* /--test3-format "%s%s" 'a 'b))
  )

(when nil
  (defclass mca () ((aaa :initarg :aaa
			 :type number
			 :documentation "slot aaa"
			 :initform 1234)
		    (bbb :initarg :bbb
			 :initform 5678
			 :documentation "slot bbb")
		    (ccc :allocation :class
			 :initform 9999
			 :documentation "slot ccc"))
    "class 1234")
  (defvar mca nil)
  (message "default:  %s" (oref-default mca ccc))
  (setq obja (mca :aaa 111 :bbb 111)) (message "obja:     %s" obja)
  (setq objb (mca :aaa 222 :bbb 222)) (message "objb:     %s" objb)
  (message "obja-ccc: %s" (oref obja ccc))
  (oset-default 'mca :aaa 1234567890)
  (setq objc (mca)) (message "objc:     %s" objc)
  (message "orefb-aa: %s" (oref objb :aaa))
  (message "orefdftb: %s" (oref-default objb :aaa))
  (cl-defgeneric mca-test (a b c) "SSSS")
  (cl-defmethod mca-test ((self mca) b c)
    "Doc string"
    (message "%s %s %s" (oref self :aaa) b c))
  (mca-test obja 100 1000)
  (/defclass tc () ((a :initarg :a))
	     "def 1234")
  (setq objd (/tc :a 11111))
  (message "%s" (oref objd :a))
  )

(when nil
  (/defun aaaaa (a b c) "funaaaaa" (message "funaaaaa"))
  (/defgeneric aaaaa (a b c) "genericaaaaa" (message "genericaaaaaa"))
  (/defmethod aaaaa ((a string) b c) "" (message "method"))
  (/aaaaa 1 2 3)
  (/aaaaa "1" 2 3)
  )

(when nil
  (message "namespace : [%s]" /--namespace)
  (message "internal  : [%s]" /--namespace*)

  (/defmacro macroa (a b c) "/defmacro" (declare (indent defun))
    `(message "/defmacro"))
  (/macroa 1 2 3)
  (/defun* funa (a b c) "/defun*" (declare (indent defun)) (message "/defun*"))
  (/--funa 1 2 3)

  (message "fmtsymbol : [%s]" (/--format 'abcd))
  (message "fmtmix    : [%s]" (/--format "%s%s" 'a "bcd"))

  (message "fmt*symbol: [%s]" (/--format* 'abcd))
  (message "fmt*mix   : [%s]" (/--format* "%s%s" 'a "bcd"))

  (message "intern    : [%s]" (/--intern "%s%s" 'a "bcd"))
  (message "intern*   : [%s]" (/--intern* "%s%s" 'a "bcd"))

  (/defmacro* macroa (a b c) "/defmacro*" (declare (indent defun))
    `(message "/defmacro*"))
  (/--macroa 1 2 3)

  (message "doc-string: [%s]" (/--doc-ref 'fun 'a 'b 'c))
  (message "doc-string: [%s]" (/--doc-ref 'fun))
  (message "doc-name  : [%s]" (/--doc-ref-name 'a 'b 'c))
  (message "doc-format: [%s]" (/--doc-ref-format 'a 'b 'c))
  (message "doc-define: [%s]" (/--doc-define 'funtion 'defun t))
  (message "doc-define: [%s]" (/--doc-define 'funtion 'defun))

  (/defun funa (a b c) "/defun" (declare (indent defun)) (message "/defun"))
  (/funa 1 2 3)
  (/defvar vara "/defvar" "/defvar") (message "%s" /vara)
  (/defvar* vara "/defvar*" "/defvar*") (message "%s" /--vara)
  (/defconst consta "/defconst" "/defconst") (message "%s" /consta)
  (/defconst* consta "/defconst*" "/defconst*") (message "%s" /--consta)
  (/defclass classa () ((aa :initform "/defclass:aa")) "/defclass")
  (message "%s" (oref-default /classa aa))
  (/defclass* classa () ((aa :initform "/defclass*:aa")) "/defclass*")
  (message "%s" (oref-default /--classa aa))
  (/defgeneric generica (a b c) "/defgeneric" (message "/defgeneric"))
  (/generica 1 2 3)
  (/defgeneric* generica (a b c) "/defgeneric*" (message "/defgeneric*"))
  (/--generica 1 2 3)
  (/defmethod generica ((a string) b c) "/defmethod" (message "/defmethod"))
  (/generica "1" 2 3)
  (/defmethod* generica ((a string) b c) "/defmethod*" (message "/defmethod*"))
  (/--generica "1" 2 3)
  
  ;; (message "%s" (/--format-custom 1234))
  ;; (message "%s" (/--intern-custom 1234))
  ;; (/defcustom customa 1234 "custom1234") (message "%s" /custom-customa)
  )

(when nil
  (/def-intern test prefix)
  (message "%s" /--namespace-test)
  (message "%s" (/--format-test "ABCD"))
  (message "%s" (/--intern-test "ABCD"))
  (message "%s" (/--intern-test* "ABCD"))
  
  (message "%s" (/path-directory "ABCD" "/A/B/C"))
  (message "%s" (/path-directory "ABCD" "A/B/C"))
  (message "%s" (/path-directory "ABCD"))
  (message "%s" (/path-user-directory "ABCD"))
  (message "%s" (/path-user-directory 'abcd))
  (message "%s" (/path-lisp-directory "ABCD"))
  (message "%s" (/--format-directory 'ABCD))
  (message "%s" (/--intern-directory 'ABCD))
  )


(when nil
  (let* ((a '(a b c))
	 (str (upcase (format "%s" a))))
    (string-match "(\\(.*\\))" str)
    (message "%s" (match-string 1 str)))
  (message "%s" (/--redirect-doc abcd a b c)))

(when nil
  (print (/regexp-quote "(abcd)"))
  (print (/regexp-quote "`..?'"))
  (print (/regexp-quote "`..?'" "(a)" "(b)"))
  (print (/regexp-quote `("(a)" "(b)")))
  ;(print (/regexp-quote "(a)" '("(A)" "(B)")))
  )

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

(when nil
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

(when nil
  (let* ((sec 1))
    (message "curr: %s %s" (current-time) (float-time))
    (message "curr: %s" (current-time-string))
    (message "add : %s %s" (time-add 1 nil) (float-time (time-add 1 nil)))
    ))

(when nil
  (/def-double-keys-event-command
   dtest
   ((/show-key-binding)
    (message "normal"))
   ((/show-key-binding)
    (message "double")) "doc")
  (/def-keys-evil-state motion C-c dtest)
  (/def-keys-evil-state motion a /show-key-binding)
  )


(/message-test-end)

(/provide)
;;; test/core.el ends here
