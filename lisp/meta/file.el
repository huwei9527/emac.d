;;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

;;; Commentary:

;;; Code:

(/require core lib meta)

(/--defformat config :format "config")

(/defmacro define-user-directory (dir &optional doc)
  "Define `/custom' variable for a directory in `user-emacs-directory'.
DIR is a `/--name' argument.
DOC is the doc-string of the variable.
\(defvar /custom-DIR-directory user-emacs-directory/DIR ...)."
  (declare (indent defun))
  (let* ((expr `(file-truename
		 (expand-file-name ,(/--name dir) user-emacs-directory))))
    (message "dir = %s; %s" dir (/--name dir))
    (/--sexp-progn-exec
      `(and (boundp '/--pre-create-directory-list)
	    (push ,expr /--pre-create-directory-list))
      `(/defcustom ((%s - directory) ,@(/--list dir)) ,expr ,doc))))

(/defmacro define-config-directory (dir &optional doc)
  "Define `/custom' variable for a directory in the `/config' directory.
DIR is a `/--name' argument.
DOC is the doc-string of the variable."
  (declare (indent defun))
  (let* ((name (/--name dir))
	 (expr `(file-truename
		 (expand-file-name
		  ,name ,(/--intern '(custom - config - directory))))))
    (/--sexp-progn-exec
      `(and (boundp '/--pre-create-directory-list)
	    (push ,expr /--pre-create-directory-list))
      `(/defcustom ((config - %s - directory) ,dir) ,expr
		   ,(format "%s `/config' directory.\n%s"
			    (capitalize name) doc)))))

(/defmacro define-file-name-predictor (name &optional doc)
  "Define a `pred' function for `/--file-name'.
NAME is a `/--name' argument.
DOC is the doc-string of the `/pred' function."
  (declare (indent defun))
  (let* ((name (/--name name))
	 (pred (/--intern '(%s - pred) name)))
    (/--sexp-progn-exec
      `(/defpred ,name)
      `(/defun ((%s - file-name - pred) ,@(/--list name)) (&optional path)
	 ,(format "`/pred' function for `/--file-name' PATH.
Make the regexp search with `/pred' function `%s'.
PATH is a `/--file-name' argument.%s" pred (if doc (format "\n%s" doc) ""))
	 (,pred (/--file-name path))))))

(/defvar* file-name-regexp-alist
  '((dotdirectory "`..?'" "system '.' and '..' directory")
    (uneditable-file "((~|#)|(.(exe|pdf|zip)))'" "emacs uneditable file")
    (system-buffer "`(*)" "system buffer")
    (temporary-buffer-prefix "`(*(Warning|AAA|BBB))"
			     "temporary buffer prefix")
    (scratch-buffer "`*scratch*'" "scratch buffer")
    (message-buffer "`*Messages*'" "message buffer")
    )
  "The alist of the filename regexp.")

(/defmacro* define-regexp ()
  "Define `/custom' variables for `/regexp' in `/--file-name-regexp-alist'."
  (declare (indent defun))
  (/--sexp-progn
    (dolist (attrs /--file-name-regexp-alist)
      (/--sexp-exec `(/defregexp ,(car attrs) ,(cadr attrs) nil 'quote)))))

(/defmacro* define-file-name-predictor ()
  "Define `/pred' functions for `/--file-name' in `/--file-name-regexp-alist'."
  (declare (indent defun))
  (/--sexp-progn
    (dolist (attrs /--file-name-regexp-alist)
      (/--sexp-exec
	`(/define-file-name-predictor ,(car attrs) ,(cl-caddr attrs))))))

(/provide)
;;; meta/file.el ends here
