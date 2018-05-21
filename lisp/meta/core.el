;;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

;;; Commentary:

;;; Code:

(defun /value (form)
  "Get the value by evaling FORM until result is not a list."
  (declare (indent defun))
  (while (listp form)
    (setq form (eval form)))
  form)

(defmacro /def-custom-var (var-or-name &optional init doc value)
  (declare (doc-string 3) (indent defun))
  (:documentation (format "Define a variable `VAR-OR-NAME' for customization.
Just like (defvar /%s-VAR-OR-NAME INIT DOC).
If VALUE is non-nil, eval INIT form." /custom-name))
  `(defvar ,(/intern-custom var-or-name)
     ,(if value (/value init) init) ,doc))

(defmacro /def-custom-const (const-or-name &optional init doc value)
  (declare (doc-string 3) (indent defun))
  (:documentation (format "Define a constant `CONST-OR-NAME' for customization.
Just like (defconst /%s-CONST-OR-NAME INIT DOC).
If VALUE is non-nil, eval INIT form." /custom-name))
  `(defconst ,(/intern-custom const-or-name)
     ,(if value (/value init) init) ,doc))

(/provide)
;;; meta/core.el ends here
