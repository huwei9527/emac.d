;;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

;;; Commentary:

;;; Code:

(/require-meta core)
(/require-lib core)
(require 'cl)

(defun /--intern-user-directory (form &rest args)
  (declare (indent defun))
  (:documentation
   (format "Use FORM as the directory format string and intern it.
The character '/' in FORM is replaced by '-' before intern it.
See `/--intern-directory'"))
  (apply #'/--intern-directory
	 (replace-regexp-in-string "/" "-" (/--name form)) args))

(defmacro /def-user-directory (form &optional doc)
  (declare (doc-string 2) (indent defun))
  (:documentation (format "Create %s and set to $HOME/.emacs.d/FORM."
			  (/--intern-user-directory 'FORM)))
  (and (boundp '/--pre-create-directory-list)
       (push (/file-user-directory form) /--pre-create-directory-list))
  `(defconst ,(/--intern-user-directory form) ,(/file-user-directory form) ,doc))

(defvar /--config-name "config"
  "The subname of configuration directory variable.")

(defun /--file-config-directory (path)
  (declare (indent defun))
  (:documentation
   (format "User configure directory path ($HOME/.emacs.d/%s/PATH)."
	   /--config-name))
  (/file-directory path (/file-user-directory /--config-name)))

(defun /--intern-config-directory (form &rest args)
  (declare (indent defun))
  (:documentation
   (format "Prepend %s to directory format string and intern it.
See `/--intern-directory'." /--config-name))
  (apply #'/--intern-directory
	 (format "%s-%s" /--config-name (/--name form)) args))

(defmacro /def-config-directory (form &optional doc)
  (declare (doc-string 2) (indent defun))
  (:documentation
   (format "Create %s and set to $HOME/.emacs.d/%s/FORM"
	   (/--intern-config-directory 'FORM) /--config-name))
  (and (boundp '/--pre-create-directory-list)
       (push (/--file-config-directory form) /--pre-create-directory-list))
  `(defvar ,(/--intern-config-directory form) ,(/--file-config-directory form)
     ,(format "%s configure directory.\n%s" (capitalize (/--name form)) doc)))

(defmacro /def-file-name-regexp (form regexp doc)
  (declare (doc-string 3) (indent defun))
  (:documentation (format "Construct a variable %s.
The variable is set to `(/file-name-regexp-quote regexp)'."
			  (/--intern-regexp 'FORM)))
  `(defvar ,(/--intern-regexp form) ,(/regexp-quote regexp) ,doc))

(defmacro /def-file-name-predictor (form doc)
  (declare (doc-string 2) (indent defun))
  (:documentation (format "Construct a predictor %s.
The predictor user regexp `%s' to check."
  (/--intern-predictor 'FORM) (/--intern-regexp 'FORM)))
  `(defun ,(/--intern-predictor form) (&optional path)
     ,doc
     (let* ((name (cond
		   ((stringp path) (file-name-nondirectory
				    (directory-file-name path)))
		   ((null path) (buffer-name))
		   ((bufferp path) (buffer-name path))
		   (t ""))))
       (string-match-p ,(/--intern-regexp form) name))))

(defvar /--file-name-regexp-alist
  '((dotdirectory "`..?'" "system '.' and '..' directory")
    (uneditable-file "((~|#)|(.(exe|pdf|zip)))'" "emacs uneditable file")
    (system-buffer "`(*)" "system buffer")
    (auto-killed-buffer "`(*(Warning|AAA|BBB))" "auto killed buffer")
    (scratch-buffer "`*scratch*'" "scratch buffer")
    (message-buffer "`*Messages*'" "message buffer")
    )
  "The alist of the filename regexp and predictor.")

(defmacro /def-file-name-regexp-all ()
  "Construct regexp variables from `/--file-name-regexp-alist'."
  (declare (indent defun))
  (/--sexp-progn
    (dolist (attrs /--file-name-regexp-alist)
      (/--sexp-exec
	`(/def-file-name-regexp ,(car attrs) ,(cadr attrs)
	   ,(format "The regular expression for %s." (caddr attrs)))))))

(defmacro /def-file-name-predictor-all ()
  "Construct regexp predictor from `/--file-name-regexp-alist'."
  (declare (indent defun))
  (/--sexp-progn
    (dolist (attrs /--file-name-regexp-alist)
      (/--sexp-exec
	`(/def-file-name-predictor ,(car attrs)
	   ,(format "Return non-nil if PATH is %s, otherwise return nil.
If PATH is buffer, the buffer name is used.
If PATH is nil, the current buffer name is used.
If PATH is a string, the filename with directory stripped is used.
If PATH is a directory, the last sub directory name is used."
		    (caddr attrs)))))))

(/provide)
;;; meta/file.el ends here
