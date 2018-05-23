;;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

;;; Commentary:

;;; Code:

(defun /--intern-format (fmt &rest args)
  "Intern the format string (format FMT @ARGS).
Just like (intern (format FMT @ARGS)), but FMT can be anything
  besides string, then the printed representation will be used instead."
  (declare (indent defun))
  (intern (apply #'format (format "%s" fmt) args)))

(defconst /--namespace-prefix "/" "Prefix of the name of user lisp code.")

(defun /--intern (fmt &rest args)
  (declare (indent defun))
  (:documentation (format "Prepend `%s' to the format string and intern it.
See `/--intern-format'." /--namespace-prefix))
  (apply #'/--intern-format (format "%s%s" /--namespace-prefix fmt) args))

(defun /--value (form &optional depth)
  "Recursively evaluate FORM at most DEPTH times.
If the FORM are evaluated to itself at some time (e.g. interger,
  string, etc), return its printed representation (format \"%s\"
  FORM).
If DEPTH is not an positive integer, set DEPTH to 65535. That is, the
  maximal depth is 65535. Note that the recursively evaluation may
  turn into infinite loop (e.g. cyclic reference), this number will
  break the loop and print the result anyhow. Try to avoid such case
  since the result is unpredictable."
  (declare (indent defun))
  (or (and (integerp depth) (> depth 0)) (setq depth 65535))
  (let* (old)
    (while (not (or (equal form old) (eq depth 0)))
      (setq old form)
      (if (and (symbolp form) (not (boundp form)))
	  (setq form nil)
	(setq form (eval form)))
      (setq depth (1- depth))))
  form)

(defun /--name (form)
  "The name of the FORM.
If FORM is symbol, return the `symbol-name'.  
Othersise, return printed representation of `(/--value FORM)'.
See `/--value'."
  (declare (indent defun))
  (or (symbolp form) (setq form (/--value form)))
  (format "%s" form))

(defun /file-directory (path &optional dir)
  "Get the absolute true PATH in DIR.
If PATH is a symbol, the `symbol-name' will be used.
If DIR is nil, the `default-directory' is used.
True path is the path following the symbolic link."
  (declare (indent defun))
  (file-truename (file-name-as-directory (expand-file-name (/--name path) dir))))

(defun /file-user-directory (path)
  "Construct directory in `user-emacs-directory' ($HOME/.emacs.d/PATH)."
  (declare (indent defun))
  (/file-directory path user-emacs-directory))

(defconst /--custom-name "custom" "The subname of custom variable.")
(defconst /--directory-name "directory" "The subname of directory variable.")
(defconst /--lisp-name "lisp" "The subname of lisp code directory variable.")

(defun /--file-lisp-directory (path)
  (declare (indent defun))
  (:documentation
   (format "User lisp code directory ($HOME/.emacs.d/%s/PATH)." /--lisp-name))
  (/file-directory path (/file-user-directory /--lisp-name)))

(defun /--intern-custom (form &rest args)
  (declare (indent defun))
  (:documentation (format "Prepend %s to user format string and intern it.
See `/--intern'." /--custom-name))
  (apply #'/--intern (format "%s-%s" /--custom-name (/--name form)) args))

(defun /--intern-directory (form &rest args)
  (declare (indent defun))
  (:documentation (format "Append %s to custom format string and intern it.
See `/--intern-custom'." /--directory-name))
  (apply #'/--intern-custom
	 (format "%s-%s" (/--name form) /--directory-name) args))

(defun /--intern-lisp-directory (form &rest args)
  (declare (indent defun))
  (:documentation
   (format "Prepend %s to the directory format string and intern it.
See `/--intern-directory'." /--lisp-name))
  (apply #'/--intern-directory
	 (format "%s-%s" /--lisp-name (/--name form)) args))

(defgroup /user nil
  "The user init file configuration group."
  :group 'convenience
  :prefix (format "%s%s" /--namespace-prefix /--custom-name))

(defmacro /def-lisp-directory (form &optional doc)
  (declare (doc-string 2) (indent defun))
  (:documentation (format "Create `%s' and `/require-FORM'.
Utility tool function to load user lisp file."
			  (/--intern-lisp-directory 'FORM)))
  (let* ((name (/--name form)) (dir (/--file-lisp-directory form)))
    `(progn
       (defconst ,(/--intern-lisp-directory form) ,dir
	 ,(format "%s code directory.\n%s" (capitalize name) doc))
       (defmacro ,(/--intern "require-%s" name) (&rest features)
	 ,(format "Load %s/FEATURE from %s directory." name name)
	 (declare (indent defun))
	 `(let* (name ft path)
	    (dolist (feature ',features)
	      (setq name (/--name feature)
		    ft (/--intern "%s/%s" ,',name name)
		    path (expand-file-name name ,',dir))
	      (or (featurep ft)
		  (if (file-exists-p (format "%s.elc" path))
		      (require ft (format "%s.elc" path))
		    (require ft (format "%s.el" path))))))
	 ;; (let* ((name (/--name feature))
	 ;; 	(ft (/--intern "%s/%s" ,name name))
	 ;; 	(path (expand-file-name name ,dir)))
	 ;;   `(unless (featurep ',ft)
	 ;;      (if (file-exists-p ,(format "%s.elc" path))
	 ;; 	  (require ',ft ,(format "%s.elc" path))
	 ;; 	(require ',ft ,(format "%s.el" path)))))
	 ))))

(defmacro /provide ()
  "Provide the feature according to the name of user loading file.
Suppose the name of the loading file is XXX/lisp/YYY/ZZZ.el, then the
file provide the feature '/YYY/ZZZ by ommiting the lisp home directory
and the file extension."
  (declare (indent defun))
  (let ((str (file-name-sans-extension load-file-name)))
    (string-match "[^/]*/[^/]*\\'" str)
    `(provide ',(/--intern (match-string 0 str)))))

(/def-lisp-directory init "Init file.")
(/def-lisp-directory config "Packages configuration.")
(/def-lisp-directory custom "User options.")
(/def-lisp-directory lib "Utility functions.")
(/def-lisp-directory meta "Utility macros.")
(/def-lisp-directory test "Test codes.")
(/def-lisp-directory tool "Batch utility tool.")

(/provide)
;;; .el ends here
