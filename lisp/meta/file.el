;;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

;;; Commentary:

;;; Code:

(defmacro /def-user-directory (sym-or-name &optional doc)
  (declare (doc-string 2) (indent defun))
  (:documentation
   (format "Create variable for the directory in the `user-emacs-directory'.
Just like `/%s-SYM-OR-NAME-%s'=$HOME/.emacs.d/SYM-OR-NAME."
  /custom-name /directory-name))
  (and (boundp '/pre-create-directory-list)
       (push (/file-user-directory sym-or-name) /pre-create-directory-list))
  `(defconst ,(/intern-directory
	       (replace-regexp-in-string "/" "-" (/name sym-or-name)))
     ,(/file-user-directory sym-or-name) ,(/name doc)))

(defvar /config-name "config"
  "The sub-name of directory variable for configuration files.")

(defun /file-config-directory (path)
  (declare (indent defun))
  (:documentation (format "Construct user configure directory path.
Just like $HOME/.emacs.d/%s/PATH" /config-name))
  (/file-directory (/name path) (/file-user-directory /config-name)))

(defun /intern-config-directory (form &rest args)
  (declare (indent defun))
  (apply #'/intern-directory
	 (format "%s-%s" /config-name (/name form)) args))

(defmacro /def-config-directory (sym-or-name &optional doc)
  (declare (doc-string 2) (indent defun))
  (:documentation
   (format "Create variable for directory of configuration files.
Just like `/%s-%s-SYM-OR-NAME-%s'=$HOME/.emacs.d/%s/SYM-OR-NAME"
	   /custom-name /config-name /directory-name /config-name))
  (and (boundp '/pre-create-directory-list)
       (push (/file-user-directory sym-or-name) /pre-create-directory-list))
  `(progn
     (defvar ,(/intern-config-directory sym-or-name)
       ,(/file-config-directory sym-or-name)
       ,(format "%s configure directory." (/name sym-or-name)))))

(/provide)
;;; meta/file.el ends here
