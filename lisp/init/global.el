;;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

;;; Commentary:

;;; Code:

(defun intern-format- (fmt &rest args)
  "Intern the format string (format FMT @ARGS).
Just like (intern (format FMT @ARGS))."
  (declare (indent defun))
  (intern (apply #'format fmt args)))

(defun /intern (fmt &rest args)
  "Intern the format string in user namespace (/).
Just append `/' to the front of the format string."
  (declare (indent defun))
  (apply #'intern-format- (format "/%s" fmt) args))

(defun /name (form)
  "Get the name by evaling the FORM.
If FORM is string, return the string.
If FORM is symbol, return the `symbol-name'.
If FORM is list, return the name of result by evaling FROM."
  (declare (indent defun))
  (while (not (stringp form))
    (cond
     ((symbolp form) (setq form (symbol-name form)))
     ((listp form) (setq form (eval form)))
     (t ignore)))
  form)

(defun /file-directory (path &optional dir)
  "Get the absolute true PATH in DIR.
PATH can be a symbol, the `symbol-name' of which will be used.
If DIR is nil, the `default-directory' is used.
True path means following all the symbolic link to the real directory."
  (declare (indent defun))
  (file-truename (file-name-as-directory (expand-file-name (/name path) dir))))

(defun /file-user-directory (path)
  "Construct directory in `user-emacs-directory'.
Just like $HOME/.emacs.d/PATH"
  (declare (indent defun))
  (/file-directory path user-emacs-directory))

(defvar /custom-name "custom"
  "The sub-name of variables and constants for user customization.")
(defvar /directory-name "directory"
  "The sub-name of directory variable for user customization.")
(defvar /lisp-name "lisp"
  "The sub-name of directory variable for source code files.")

(defun /file-lisp-directory (path)
  (declare (indent defun))
  (:documentation (format "Construct user code directory path.
Just like $HOME/.emacs.d/%s/PATH" /lisp-name))
  (/file-directory (/name path) (/file-user-directory /lisp-name)))

(defun /intern-custom (form &rest args)
  (declare (indent defun))
  (:documentation (format "Intern FORM as user custom variable.
Just like `/%s-FORM'.
If FORM is a symbol, ARGS must be nil and the `symbol-name' of FORM is used.
If FORM is string, FORM can be a format string and ARGS is the format
  parameter." /custom-name))
  (apply #'/intern (format "%s-%s" /custom-name (/name form)) args))

(defun /intern-directory (form &rest args)
  (declare (indent defun))
  (:documentation (format "Intern FORM as user directory variable.
Just like `/%s-FORM-%s'.
see `/intern-custom' for paramter explanation." /custom-name /directory-name))
  (apply #'/intern-custom (format "%s-%s" (/name form) /directory-name) args))

(defun /intern-lisp-directory (form &rest args)
  (declare (indent defun))
  (:documentation (format "Intern FORM as user code directory variable.
Just like `/%s-list-FORM-%s'.
see `/intern-custom' for paramter explanation." /custom-name /directory-name))
  (apply #'/intern-directory (format "%s-%s" /lisp-name (/name form)) args))

(defgroup /user nil
  "The user init file configuration group."
  :group 'convenience
  :prefix (format "/%s" /custom-name))

;; (defmacro /def-custom-var (var-or-name &optional init doc)
;;   (declare (doc-string 3) (indent defun))
;;   (:documentation (format "Define a variable `VAR-OR-NAME' for customization.
;; Just like (defvar /%s-VAR-OR-NAME INIT DOC)." /custom-name))
;;   `(defvar ,(/intern-custom var-or-name) ,(/value init) ,doc))

;; (defmacro /def-custom-const (const-or-name &optional init doc)
;;   (declare (doc-string 3) (indent defun))
;;   (:documentation (format "Define a constant `CONST-OR-NAME' for customization.
;; Just like (defconst /%s-CONST-OR-NAME INIT DOC)" /custom-name))
;;   `(defconst ,(/intern-custom const-or-name) ,(/value init) ,doc))

;; (defmacro /def-user-directory (sym-or-name &optional doc)
;;   (declare (doc-string 2) (indent defun))
;;   (:documentation
;;    (format "Create variable for the directory in the `user-emacs-directory'.
;; Just like `/%s-SYM-OR-NAME-%s'=$HOME/.emacs.d/SYM-OR-NAME."
;;   /custom-name /directory-name))
;;   (and (boundp '/pre-create-directory-list)
;;        (push (/file-user-directory sym-or-name) /pre-create-directory-list))
;;   `(defconst ,(/intern-directory
;; 	       (replace-regexp-in-string "/" "-" (/name sym-or-name)))
;;      ,(/file-user-directory sym-or-name) ,(/name doc)))

;; (/def-user-directory (eval /lisp-name) "User code directory.")

(defmacro /def-lisp-directory (sym-or-name &optional doc)
  (declare (doc-string 2) (indent defun))
  (:documentation
   (format "Create `/%s-%s-SYM-OR-NAME-%s' and `/require-SYM-OR-NAME'.
This is used user file load."
	   /custom-name /lisp-name /directory-name))
  `(progn
     (defconst ,(/intern-lisp-directory sym-or-name)
       ,(/file-lisp-directory sym-or-name)
       ,(format "%s code directory. %s"
		(capitalize (/name sym-or-name)) doc))
     (defmacro ,(/intern "require-%s" (/name sym-or-name)) (feature)
       ,(format "Load %s/FEATURE file from %s directory."
		(/name sym-or-name) (/name sym-or-name))
       (declare (indent defun))
       (let* ((ft (/intern "%s/%s" ,(/name sym-or-name) (/name feature)))
	      (path (expand-file-name (/name feature)
				      ,(/file-lisp-directory sym-or-name))))
	 `(unless (featurep ',ft)
	    (if (file-exists-p ,(format "%s.elc" path))
		(require ',ft ,(format "%s.elc" path))
	      (require ',ft ,(format "%s.el" path))))))))

(/def-lisp-directory init "Specific init file.")
(/def-lisp-directory config "Configure packages.")
(/def-lisp-directory custom "Customization options.")
(/def-lisp-directory lib "Useful functions.")
(/def-lisp-directory meta "Code generators (macro).")
(/def-lisp-directory test "Test codes.")
(/def-lisp-directory tool "Useful batch mode tool.")

(provide '/init/global)
;;; .el ends here
