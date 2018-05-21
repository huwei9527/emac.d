;;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

;;; Commentary:

;;; Code:

(defmacro /def-custom-var (var-or-name &optional init doc depth)
  (declare (doc-string 3) (indent defun))
  (:documentation (format "Define a variable `VAR-OR-NAME' for customization.
Just like (defvar /%s-VAR-OR-NAME INIT DOC).
If VALUE is non-nil, eval INIT form." /custom-name))
  `(defvar ,(/intern-custom var-or-name)
     ,(if depth (/value init depth) init) ,doc))

(defmacro /def-custom-const (const-or-name &optional init doc depth)
  (declare (doc-string 3) (indent defun))
  (:documentation (format "Define a constant `CONST-OR-NAME' for customization.
Just like (defconst /%s-CONST-OR-NAME INIT DOC).
If VALUE is non-nil, eval INIT form." /custom-name))
  `(defconst ,(/intern-custom const-or-name)
     ,(if depth (/value init depth) init) ,doc))

(/provide)
;;; meta/core.el ends here
