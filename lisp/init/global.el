;;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

;;; Commentary:

;;; Code:

(require 'macroexp)
(require 'cl-lib)

;; /--name: Only used in global.el file.
(defun /--name (form)
  "Return `/--name' of FORM.
If FORM is a list, return the string by applying it to `format'.
Otherwise return the string of printed presentation."
  (if (listp form) (apply #'format form) (format "%s" form)))

;;; {{ Define /define and /define* macros.
;; Define namespace /--namespace.
(defconst /--namespace "/" "The prefix of `/--namespace'.")
;; Define namespace /--namespace*.
(defconst /--namespace* (format "%s--" /--namespace)
  "The prefix of `/--namespace*'.")

;; Define /define and /define* macros.
(defun /--def--defmacro-form (name args &optional internal)
  "Return defmacro form to define `define' macro.
NAME is the symbol of original `define' macro or function.
If INTERNAL is nil, define `/define' macro, otherwise define `/define*' macro.
ARGS is the argument list of the original macro without the first argument.

The name of `/define' macro is /name.
The name of `/define*' macro is /name*.

As a special case, if NAME is a quote form, the name symbol `/name' and
`/name*' is also quoted. This is used in `define' function.

For example,
  (/--def--defmacro-form 'defun (args &optional doc decls &rest body))
    => macro /defun
       (/defun abcd (a b) \"\" nil) => function /abcd
  (/--def--defmacro-form ''defalias (def &optional doc) 'internal)
    => macro /defalias*
       (/defalias* abcd 'message \"\") => function alias /--abcd"
  (let* ((quotep (eq (car-safe name) 'quote))
	 (ns (if internal /--namespace* /--namespace))
	 (decls (if (memq (car-safe (car args))
			  '(declare interactive cl-declare)) (pop args)
		  '(declare (indent defun))))
	 (rargs args) sargs)
    (and quotep (setq name (cadr name)))
    ;; (xxx &rest body):  sargs <= xxx; rargs <= body
    (while (and rargs (not (memq (car-safe rargs) '(&rest &body))))
      (or (eq (aref (symbol-name (car-safe rargs)) 0) ?\&)
	  (push (car rargs) sargs))
      (setq rargs (cdr rargs)))
    (and sargs (setq sargs (nreverse sargs)))
    (and rargs (setq rargs (cadr rargs)))
    `(defmacro ,(intern (format (if internal "%s%s*" "%s%s") /--namespace name))
	 (name ,@args)
       ,(format "Define `%sNAME' with `%s'.
NAME is a /--name argument. The name symbol is interned in `%s'.
The rest arguments are the same as `%s'.
For example,
 (%s%s abcd ...) => (%s %sabcd ...)"
		ns name (if internal '/--namespace* '/--namespace)
		name ns name name ns)
       ,decls
       (append `(,',name
       		 ,(,(if quotep #'macroexp-quote #'identity)
		   (intern (format "%s%s" ,ns (/--name name)))))
       	       (list ,@sargs) ,rargs))))

(defmacro /--def (name &rest args)
  "Define `/define' and `/define*' macro.
NAME is the symbol of original `define' macro.
ARGS is the argument list of original macro without the first argument."
  `(progn ,(/--def--defmacro-form name args)
	  ,(/--def--defmacro-form name args 'internal)))

;; /--format is not setup so `define' name can't use /--format name
;; Use symbol , string or number
(/--def defmacro args &optional doc &rest body)
(/--def defun args &optional doc &rest body)
(/--def defsubst args &rest body)
(/--def defconst init &optional doc)
(/--def defvar init &optional doc)
(/--def defclass superclasses slots &rest body)
(/--def define-minor-mode doc &optional init lighter keymap &rest body)
(/--def defface spec doc &rest args)
(/--def 'defalias def &optional doc)
(/--def cl-defgeneric args &rest body)
(/--def cl-defmethod args &rest body)
(/--def cl-defun args &rest body)
(/--def cl-defmacro arg &rest body)
;;; }}

(/defmacro* error-unsupported-type (&rest args)
  "Raise a error of unsupported type"
  (declare (indent defun))
  `(error "Unsupported type %s" ,(if (> (length args) 1) args (car args))))

;; /--format: Only used in global.el
(/defalias* format #'format "Return the `/--format' string.
Just return the format-string with `format'.")

(/defmacro* intern-internal (namespace format &rest args)
  "Intern the `/--format' string in NAMESPACE.
NAMESPACE is the prefix of the interned symbol.
FORMAT is a `/--format' argument."
  `(intern (format "%s%s" ,namespace (/--format ,format ,@args))))

(/defmacro* intern (format &rest args)
  "Intern the `/--format' string in `/--namespace'."
  `(/--intern-internal ,/--namespace ,format ,@args))

(/defmacro* intern* (format &rest args)
  "Intern the `/--format' string in `/--namespace*'."
  `(/--intern-internal ,/--namespace* ,format ,@args))

(/defconst* lisp-directory "lisp")

;;; {{ /--feature and /--module
(/defmacro provide ()
  (:documentation
   (format "Provide `/--feature' in `/--module' according the `load-file-name'.
If `load-file-name' is `.../MODUAL/FEATURE.el[c]', then the `/--feature'
provided is `%sMODUAL/FEATURE'." /--namespace))
  (declare (indent defun))
  `(let ((str (file-name-sans-extension load-file-name)))
     (string-match "\\([^/]*\\)/\\([^/]*\\)\\'" str)
     (provide (/--intern "%s/%s" (match-string 1 str) (match-string 2 str)))))

;; Define /--feature and /--module
(/defmacro* defrequire (module &optional doc)
  (declare (indent defun))
  (:documentation (format "Define macro to load `/--feature' from MODULE.
A `/--module' is a set of lisp files for special purpose.
For example,
  custom - customization for packages
  meta   - macros to generate codes

A `/--module' is a directory of the same name in `user-emacs-directory/%s'.

Each file in a `/--module' directory provides a `/--feature' which is the
same name as the file name.

A `/--module' can provide many features.
A `/--feature' of the same name can be in different `/--module'.

Loading a `/--feature' in a `/--module' is to load the corresponding file
`user-emacs-directory/%s/MODULE/FEATURE.el[c]'."
			  /--lisp-directory /--lisp-directory))
  (let* ((name (/--name module))
	 (dir-format `("%s-%s-directory" ,/--lisp-directory ,name)))
    `(progn
       (/defconst ("%s-%s-directory" ,/--lisp-directory ,name)
	 (expand-file-name ,name (expand-file-name
				  ,/--lisp-directory
				  (file-truename user-emacs-directory)))
	 ,(format "The directory of `/--module %s'." name))
       (/defmacro ("require-%s" ,name) (&rest features)
	 ,(format "Load FEATURES in `/--module %s'.%s"
		  name (if doc (format "\n%s" doc) ""))
	 (declare (indent defun))
	 `(dolist (feature ',features)
	    (let* ((name (/--name feature)))
	      (require (/--intern "%s/%s" ,',name name)
		       (expand-file-name name
					 ,',(/--intern "%s-%s-directory"
						       /--lisp-directory
						       name)))))))))

;; Define modules
(/--defrequire init "Init Emacs in different environments.")
(/--defrequire config "Configure Emacs packages.")
(/--defrequire custom "Customize the configure module.")
(/--defrequire lib "Useful functions and emacs commands.")
(/--defrequire meta "Useful macros to generate configurations.")
(/--defrequire test "Test code.")
(/--defrequire tool "Usefule lisp batch code.")
;;; }}

(/provide)
;; (provide '/init/global)

;;; global.el ends here
