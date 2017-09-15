;; -*- lexical-binding : t ;byte-compile-dynamic : t -*-

(require 'test-code)
(require 'sequence-lib)

(defmacro intern-format (ft &rest args)
  "Extend 'intern' with format FT string."
  `(intern (format ,ft ,@args)))

;;; {{ Helper macros to construct SEXP forms for 'defmacro'
;;;    Note: SEXP are evaluated once. e.g '(code-item SEXP)'
;;;          will evaluate SEXP and add the result to the
;;;          structure form like 'progn', 'cond' ... If you
;;;          want SEXP to be added literally, you have to
;;;          quote it like '(code-item 'SEXP)'
(defvar code--obarray (make-vector 11 0))
(defsubst code--gensym (name)
  "Create a symbol with name NAME interned in 'code--obarray'."
  (intern name code--obarray))

(defsubst code--gensym-sexp-list ()
  "Create a temporary symbol interned in 'code--obarray'."
  (code--gensym "code--sexp-list"))

(defmacro code-sexp (&rest body)
  "Create a form sexp."
  (declare (indent defun))
  (let* ((tmp (code--gensym-sexp-list)))
    `(let* ((,tmp nil))
       ,@body
       (nreverse ,tmp))))

(defmacro code--append-one (el &optional eval-p)
  "Append EL to form list."
  `(push ,el ,(code--gensym-sexp-list)))

(defmacro code-append (&rest els)
  "Append body to form list."
  (code-progn
   (dolist (el els)
     (code--append-one
      `(code--append-one ,el)))))

(defmacro code-progn (&rest body)
  "Create a 'progn' form."
  `(code-sexp
    (code--append-one 'progn)
    ,@body))

(defmacro code-item (&rest sexps)
  "Create sexp form in 'progn' form."
  `(code-append ,@sexps))

(defmacro code-cond (&rest body)
  "Create a 'cond' form."
  `(code-sexp
    (code--append-one 'cond)
    ,@body))

(defmacro code-case (cond-sexp &rest body)
  "Create a 'case' form in 'cond' form."
  `(code-append
    (code-sexp (code-append ,cond-sexp ,@body))))

(defmacro code-setq (&rest body)
  "Create a 'setq' form."
  `(code-sexp
    (code--append-one 'setq)
    ,@body))

(defmacro code-pair (key value &rest pairs)
  "Create a 'key value' item in 'setq' form."
  (code-progn
   (code-item `(code-append ,key ,value))
   (while pairs (code-item `(code-pair ,(pop pairs) ,(pop pairs))))))

(defun code-parse-list (sexp op)
  "Parse the list, take operator on each element recursively."
  (let* ((stack (list (cons sexp nil)))
	 stack-curr sexp-curr sexp-parent sexp-parser
	 path op-rlt)
    (while stack
      (setq stack-curr (pop stack)
	    sexp-curr (car stack-curr)
	    sexp-parent (cdr stack-curr))
      (while (not (eq sexp-parent (car path)))
	(pop path))
      (when (and (funcall op sexp-curr path)
		 (listp sexp-curr))
	(setq sexp-parser sexp-curr)
	(while sexp-parser
	  (push (cons (car sexp-parser) sexp-curr) stack)
	  (setq sexp-parser (cdr sexp-parser))))
      (push sexp-curr path))))

(defvar code-macro-list nil
  "The list of macros defined in 'code'.")
(defmacro code-record-macro (macro)
  "Record MACRO in 'code-macro-list'"
  `(push ',macro code-macro-list))
(defmacro code-expandmacro-p (macro)
  "Non-nil if MACRO is in 'code-macro-list'"
  (if (symbolp macro)
      `(memq ',macro code-macro-list)
    `(memq ,macro code-macro-list)))
(defun code-expandmacro (sexp pos path)
  "Expand SEXP if it is a 'code' macro."
  (when (listp sexp)
    (let* ((sexp-head (car sexp)))
      (when (and (not (listp sexp-head))
		 (string-prefix-p "code-" (symbol-name sexp-head))
		 (not (code-expandmacro-p (car sexp))))
	(display-warning 'code-not-tracked
			 (format "code macro not tracked: %s" sexp-head)))))
  (if (and (listp sexp) (code-expandmacro-p (car sexp)))
      (progn (setf (elt (car path) pos) (macroexpand-all sexp)) nil)
    t))

(defmacro code-eval-after-load (package &rest body)
  "Run body after PACHAGE loads.

This extend 'eval-after-load' to 'code' macros which is useful
in byte compilation."
  (code-sexp
    (code-append 'eval-after-load)
    (if (symbolp package)
	(code-append `(quote ,package))
      (code-append package))
    (sequence-element-filter body 'code-expandmacro)
    (code-append
     `(quote
       ,(code-progn
	 (dolist (form body)
	   (code-item form)))))))
(code-record-macro code-eval-after-load)
;; }}

(defmacro code-define-regexp (name fil &optional type)
  "Define a regular expression from FIL with TYPE."
  (let* ((str-name (symbol-name name))
	 (str-fil (replace-regexp-in-string "[()|.*]" "\\\\\\&" fil))
	 sym-regexp sym-regexp-fun str-doc)
    (cond
     ((eq type 'head)
      (setq sym-regexp (intern-format "%s-head-regexp" str-name)
	    str-doc "head "))
     ((eq type 'tail)
      (setq sym-regexp (intern-format "%s-tail-regexp" str-name)
	    str-doc "tail "))
     (t
      (setq sym-regexp (intern-format "%s-regexp" str-name)
	    str-doc "")))
    `(progn
       (defconst ,sym-regexp
	 ,(cond
	   ((eq type 'head) (format "\\`%s" str-fil))
	   ((eq type 'tail) (format "%s\\'" str-fil))
	   (t str-fil))
	 ,(format "The %sregular expression for %s"
		  str-doc str-name))
       (defun ,(intern-format "%s-p" (symbol-name sym-regexp)) (str)
	 ,(format "Return non-nil if STR match %sregexp '%s'."
		  str-doc (symbol-name sym-regexp))
	 (string-match-p ,sym-regexp str)))))

(defmacro code-defregexp-head (name fil)
  "Define a head regular expression from FIL"
  `(code-define-regexp ,name ,fil head))

(defmacro code-defregexp-tail (name fil)
  "Define a tail regular expression from FIL"
  `(code-define-regexp ,name ,fil tail))

(provide 'code)
;; code.el ends here
