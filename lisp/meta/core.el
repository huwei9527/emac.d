;;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

;;; Commentary:

;;; Code:

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

;;; {{ private temporary symbol
(defvar /obarray (make-vector 107 0)
  "Private obarray to store interned symbol. 
These symbol can be get by name and won't conflict with any normal
interned symbols.")

(defun /gensym (name)
  "Intern a symbol of name NAME in `/obarray'."
  (declare (indent defun))
  (intern name /obarray))
;;; }}

;;; {{ sexp constructor.

;;; }}

(/provide)
;;; meta/core.el ends here
