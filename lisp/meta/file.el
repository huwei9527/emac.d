;;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

;;; Commentary:

;;; Code:

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

(/provide)
;;; meta/file.el ends here
