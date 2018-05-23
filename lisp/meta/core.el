;;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

;;; Commentary:

;;; Code:

(/require-lib core)

(defun /--string (form)
  "Evaluate FORM adaptively until FORM is a string.
The function assumes that form will eventually turn into a string."
  (declare (indent defun))
  (while (not (stringp form)) (setq form (eval form))) form)

(defun /--list (form)
  "Evaluate FORM adaptively until FORM is a list.
The function assumes that form will eventually turn into a string."
  (declare (indent defun))
  (while (not (listp form)) (setq form (eval form))) form)

;;; {{ Create variable with /custom prefix
(defmacro /def-custom-var (form &optional init doc depth)
  (declare (doc-string 3) (indent defun))
  (:documentation (format "Define a variable (defvar %s INIT DOC).
If DEPTH is non-nil, (/--value INIT DEPTH)." (/--intern-custom 'FORM)))
  `(defvar ,(/--intern-custom form)
     ,(if depth (/--value init depth) init) ,doc))

(defmacro /def-custom-const (form &optional init doc depth)
  (declare (doc-string 3) (indent defun))
  (:documentation (format "Define a constant (defconst %s INIT DOC).
If DEPTH is non-nil, (/--value INIT DEPTH)." (/--intern-custom 'FORM)))
  `(defconst ,(/--intern-custom form)
     ,(if depth (/--value init depth) init) ,doc))
;;; }}

;;; {{ sexp constructor.
(defvar /--sexp-list (make-symbol "/--sexp-list---")
  "Store the list of sexp during the sexp construction")
(set /--sexp-list nil)

(defmacro /--sexp (&rest body)
  "Create a sexp from BODY.
The sexp is store in `/--sexp-list' and `/--sexp-list' is set to nil
  before evaluate BODY."
  (declare (indent defun))
  `(let* ((,/--sexp-list nil))
     ,@body
     (setq ,/--sexp-list (nreverse ,/--sexp-list))
     ; (print ,/--sexp-list)
     ,/--sexp-list))

(defmacro /--sexp-append-1 (expr)
  "Append EXPR to `/--sexp-list'."
  (declare (indent defun))
  `(push ,expr ,/--sexp-list))

(defmacro /--sexp-append (&rest exprs)
  "Append EXPRS to `/--sexp-list'."
  (declare (indent defun))
  ; (/--sexp-progn (dolist (e exprs) (/--sexp-append-1 `(/--sexp-append-1 ,e))))
  `(setq ,/--sexp-list (apply #'/prepend ,/--sexp-list ,@exprs nil))
  )

(defalias '/--sexp-exec #'/--sexp-append "Append SEXPS to `pron' list.")

(defmacro /--sexp-progn (&rest body)
  "Create a `progn' form."
  (declare (indent defun))
  `(/--sexp (/--sexp-append-1 'progn) ,@body))

(defmacro /--sexp-progn-exec (&rest body)
  "Create a `progn' form.
BODY is wrapped in `/--sexp-exec' form so other constructor has no effect."
  (declare (indent defun))
  `(/--sexp-progn (/--sexp-exec ,@body)))

(defmacro /--sexp-cond (&rest body)
  "Create a `cond' form."
  (declare (indent defun))
  `(/--sexp (/--sexp-append-1 'cond) ,@body))

(defmacro /--sexp-cond-case (&rest body)
  "Create a `cond' form.
BODY is wrapped in `/--sexp-exec' form so other constructor has no
  effect. BODY should be a list of form (csexp forms)."
  `(/--sexp-cond (/--sexp-exec ,@body)))

(defmacro /--sexp-case (csexp &rest body)
  "Create a `case' form."
  (declare (indent defun))
  `(/--sexp-append (,csexp ,@body)))
;;; }}

(/provide)
;;; meta/core.el ends here
