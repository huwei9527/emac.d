;;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

;;; Commentary:

;;; Code:

;(/require-lib core)
(/require-meta core)

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
  (message "string: %s" (/--name "abcd"))
  (message "number: %s" (/--name 1234))
  (message "symbol: %s" (/--name 'abcd))
  (message "list:   %s" (/--name '((a b c d))))
  )

(when nil
  (message "%s" (/--quotep '(quote a)))
  (message "%s" (/--quotep '(function a)))
  (message "%s" (/--quotep '(cl-function a)))
  (message "quote symbol: %s" (/--quote 'a))
  (message "quote quote:  %s" (/--quote ''a))
  (message "unquote:      %s" (/--unquote ''a))
  (message "unquote many: %s" (/--unquote '#''#'(cl-function a)))
  )

(when nil
  (message "%s" (/--list 'a))
  (message "%s" (/--list ''a)))

(when nil
  (message "%s" (/--symbol '((custom - %s - directory) "%s" "sample"))))

(when nil
  (message "%s" (/--file-name "/a/b/c/aaaa"))
  (message "%s" (/--file-name nil))
  (message "%s" (/--file-name (save-excursion (next-buffer))))
  (message "%s" (/--buffer-file-name))
  )

(when nil
  (message "nil:     %s" (/--function nil))
  (message "symbol:  %s" (/--function '+))
  (message "default: %s" (/--function nil '(+ 1 2 3 4)))
  (message "list:    %s" (/--function '(+ 100 200 300 400)))
  (/--funcall '(message 1 2 3 4) "%s%s%s%s%s%s" 0 "X")
  (/--apply '(message 1 2 3 4) "%s%s%s%s%s%s" 0 '("X")))

(when nil
  (/--defformat fmtM :format "M"
		:func (lambda (s &optional b) (format "M%sM" s)))
  (/--defformat fmtN :format '("N" %s "N"))
  (/--defformat fmtP :format '((fmtN) ("P" %s "P")))
  (message "%s" (/--format '("A" 1 fmtM)))
  (message "%s" (/--format '(fmtM %s (fmtM)) "%s%s" "XX" "YY"))
  (message "%s" (/--format '("A" fmtN "B") "XXX"))
  (message "%s" (/--format '(fmtP) "XXX"))
  (message "%s" (/--format '(a b c d)))
  )

(when nil
  (/defregexp abcd "`a(b|d)" "doc" 'quote)
  (/defpred abcd "pred doc")
  (message "%s" /custom-abcd-regexp)
  (message "%s" (/abcd-p "abccc"))
  (message "%s" (/abcd-p "adccc"))
  (message "%s" (/abcd-p "aeccc"))
  )

(when nil
  ;(/mute-call #'message "%s" 1234)
  (/mute-call (lambda () (message "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX")))
  (message "neq: %s" (/neq 'a 'a))
  (message "nequal: %s" (/nequal '(1 2 3 4) '(1 2 3 4)))
  )

(when nil
  (message "prepend: %s" (/prepend '(1) '(2 3 4 5)))
  (message "push: %s" (/push '(1 2 3 4) 5 6 7 8))
  (message "quote: %s" (/seq-quote '(a b c d e 1 2 3)))
  (message "pick-quote: %s"
	   (/seq-pick-quote '(a b c d e 1 2 3) (lambda (_e i)
						 (cl-oddp i))))
  (message "quote-even: %s" (/seq-quote-even '(a b c d e f)))
  (message "quote-odd : %s" (/seq-quote-odd '(a b c d e f)))
)

(when nil
  (message "%s" (/seq-map-alternate '(1 2 3 4)
				    (lambda (e) (format "first: %s" e))
				    (lambda (e) (format "second: %s" e)))))

(when nil
  (message "%s" (/--sexp (set /--sexp--form-symbol 124)))
  (message "%s" (/--sexp (/--sexp-add 1 2 3 4)
		  (/--sexp-add '(print 1) '(print 2) '(print 3))))
  (message "%s" (/--sexp (/--sexp-quote-add (print 1) (print 2) (print 3))))
  (message "%s" (/--sexp (/--sexp-quote-add-even
  			  (message "%s" "aaa")
  			  (message "%s" "bbb")
  			  (message "%s" "ccc"))))
  (message "%s" (/--sexp (/--sexp-append `((message "aaa")
					   (message "bbb")
					   (message "ccc"))
			   `((message "111") (message "222") (message "333")))))
  (message "%s" (/--sexp-progn (/--sexp-exec '(message "1") '(message "2"))))
  (message "%s" (/--sexp-cond-case `(t (message "aaa")) `(nil (message "ccc"))))
  (message "%s" (/--sexp-cond (/--sexp-case '(eq a 1) '(print 1) '(print 2))
  		  (/--sexp-case '(eq a 2) '(print 11) '(print 22))))
  (message "%s" (/--sexp-setq (/--sexp-pair a '(format "aaaa")
				b '(format "bbbb"))))
  (message "%s" (/--sexp-setq-pair a '(format "aaa") b '(format "bbb")))
  )

(when nil
  (/ppmacroexpand (/require elpa lib init meta)))

(when nil
  (/eval-after-load package (message "1") (message "2")
    (message "3")))

(when nil
  (message "%s" (/regexp-quote "abcd"))
  (message "%s" (/regexp-quote "`(a)(b).*'"))
  (message "%s" (/regexp-quote "(a)" "(b)" "|c|"))
  (message "%s" (/regexp-quote '("(a)" "(b)" "|c|")))
  (message "%s" (/regexp-quote '(("(a)" "(b)" cd|e) "(d)" ("(e)" 123))))
  )

(when nil
  (defvar sss (symbol-function #'message))
  (message "xxx: %s" sss)
  (defmacro mtt ()
    ""
    `(funcall ,sss "%s %s" 12 34))
  ;(/ppmacroexpand (mtt))
  (mtt)
  (defvar a 11111)
  (defvar b 11111)
  (defvar c 11111)
  (let* (a b c)
    (cl-multiple-value-setq (a b c) '(100 200 300))
    (message "%s %s %s" a b c))
  (message "%s %s %s" a b c)
  )


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
