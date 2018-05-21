;;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

;;; Commentary:

;;; Code:

;; (setq /pre-create-directory-list nil)

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

(/def-user-directory (eval /config-name)
  "The directory for user configuration files.")

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

(/def-config-directory auto-save "The directory")
;; (pp /pre-create-directory-list)


(provide '/meta/file)
;;; meta/file.el ends here
