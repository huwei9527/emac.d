;;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

;;; Commentary:

;;; Code:

(require 'macroexp)
(require 'cl-lib)
(require 'eieio)

(defconst /--namespace "/" "The name prefix of user symbol.")
(defconst /--namespace* "--" "The name prefix of user internal symbol.")

;;; {{ User namespace defining tools.
(defmacro /--def (name &rest args)
  (:documentation (format "Define two macros in the user namespace.
\(/NAME name ...) => `%sname' (normal object)
\(/NAME* name ...) => `%s%sname' (internal object)

NAME and the arguments `name' in both macros are filtered by
`(/--name NAME)'. See `/--name' for detail.
The rest arguments of both macros are the same as the original `NAME' macro."
			  /--namespace /--namespace /--namespace*))
  (let* ((name (intern (/--name name))) pargs sargs rest decls)
    (and (eq (car-safe (car args)) '(declare interactive cl-declare))
	 (setq decls (car args) args (cdr args)))
    (setq pargs (cl-copy-list args))
    (while (and pargs (not (memq (car-safe pargs) '(&rest &body))))
      (or (eq (aref (symbol-name (car-safe pargs)) 0) ?\&)
	  (push `,(car pargs) sargs))
      (setq pargs (cdr pargs)))
    (setq sargs (nreverse sargs) rest (cadr pargs) pargs (cddr pargs))
    (macroexp-progn
     `((defmacro ,(intern (format "%s%s" /--namespace name))
	   (name ,@args)
	 ,(format "Define `%sNAME' with `%s' in user namespace.\n
NAME is filtered by `(/--name NAME)'. See `/--name' for detail.
The rest arguments are the same as `%s'." /--namespace name name)
	 ,(if decls decls '(declare (indent defun)))
	 (append `(,',name
		   ,(intern (format "%s%s" /--namespace (/--name name))))
		 (list ,@sargs) ,rest (list ,@pargs)))
       (defmacro ,(intern (format "%s%s*" /--namespace name))
	   (name ,@args)
	 ,(format "Define `%s%sNAME' with `%s' in user internal namespace.\n
NAME is filtered by `(/--name NAME)'. See `/--name' for detail.
The rest arguments are the same as `%s'." /--namespace /--namespace* name name)
	 ,(if decls decls '(declare (indent defun)))
	 (append `(,',name
		   ,(intern (format "%s%s%s" /--namespace /--namespace*
				    (/--name name))))
		 (list ,@sargs) ,rest (list ,@pargs)))
       ))))

(/--def defmacro args &optional doc &rest body)
(/--def defun args &optional doc &rest body)
(/--def defconst init &optional doc)
(/--def defvar init &optional doc)
(/--def defclass superclasses slots &rest body)
(/--def cl-defgeneric args &rest body)
(/--def cl-defmethod args &rest body)
(/--def cl-defun args &rest body)
(/--def cl-defmacro arg &rest body)
;;; }}

(/defun* doc-name (name)
  "Return the usage doc string for `name' arguments."
  (let* ((str (upcase (/--name name))))
    (format "If %s is a symbol, the `symbol-name' is used; otherwise the printed
  representation of `(/--value %s)' is used." str str)))

(/defun* doc-format-arguments (fmt)
  ""
  (format "%s\nThe rest arguments are the same as `format'." (/--doc-name fmt)))

(/defun* doc-format (fmt intern internal)
  "Return the usage doc string for `format' functions."
  (format "%s the string in user %snamespace `%sFMT-STRING'.\n\n%s
The rest arguments are the same as `format'."
	  (if intern "Intern" "Format")
	  (if internal "internal " "")
	  (if internal (format "%s%s" /--namespace /--namespace*) /--namespace)
	  (/--doc-name 'fmt)))

(/defun* format (fmt &rest args)
  (:documentation (/--doc-format 'fmt nil nil))
  (apply #'format (concat /--namespace (/--name fmt)) args))

(/defun* format* (fmt &rest args)
  (:documentation (/--doc-format 'fmt nil 'internal))
  (apply #'format (concat /--namespace* (/--name fmt)) args))

(/defun* intern (fmt &rest args)
  (:documentation (/--doc-format 'fmt 'intern nil))
  (intern (apply #'/--format fmt args)))

(/defun* intern* (fmt &rest args)
  (:documentation (/--doc-format 'fmt 'intern 'internal))
  (/--intern (apply #'/--format* fmt args)))

(/defclass* format-class ()
  ((prefix :initarg :prefix
	   :type string
	   :documentation "Prefix string."
	   :initform "")
   (suffix :initarg :suffix
	   :type string
	   :documentation "Suffix string."
	   :initform ""))
  "The class of format string setting.")

(/defconst* base-format (/--format-class))
(/defconst* custom-format (/--format-class :prefix "custom"))

(/cl-defgeneric* format-internal (format fmt &rest args)
  "Format string using `FORMAT' and `FMT ARGS'.
First format string using `format' with arguments `FMT ARGS'.
If FMT is symbol, the `symbol-name' is used; otherwise the printed
  representation of `(/--value FMT)' is used.
The rest ARGS are the same as `format'.
Then add prefixes and suffixes to the string according to FORMAT."
  (:method ((self /--format-class) fmt &rest args)
	   "FORMAT is a instance of  `/--format-class'."
	   (let* ((prefix (oref self prefix)) (suffix (oref self suffix)))
	     (format "%s%s%s"
		     (if (eq prefix "") "" (format "%s-" prefix))
		     (apply 'format (/--name fmt) args)
		     (if (eq suffix "") "" (format "-%s" suffix)))))
  (:method ((selfs list) fmt &rest args)
	   "FORMAT is a list of instances of `/--format-class'."
	   (let* ((str (apply 'format (/--name fmt) args)))
	     (while selfs (setq str (/--format-internal (car selfs) str)
				selfs (cdr selfs)))
	     str))
  (:method ((_default (eql nil)) fmt &rest args)
	   "FORMAT is set to default instance `/--base-format'."
	   (apply '/--format-internal /--base-format fmt args))
  (:method (format _fmt &rest _args)
	   "Fall through method, singal an error."
	   (error "Unknown format %s" format)))

(/defun* fmt (fmts fmt &rest args)
  ""
  (format "%s%s" /--namespace (apply '/--format-internal fmts fmt args)))

(/defun* fmt* (fmts fmt &rest args)
  ""
  (format "%s%s%s" /--namespace /--namespace*
	  (apply '/--format-internal fmts fmt args)))

(/defun* int (fmts fmt &rest args)
  ""
  (intern (apply '/--fmt fmts fmt args)))

(/defun* int* (fmts fmt &rest args)
  ""
  (intern (apply '/--fmt* fmts fmt args)))

(/defconst* namespace-format "format" "The name prefix of format symbol.")
(/defconst* namespace-intern "intern" "The name prefix of intern symbol.")
(/defconst* namespace-namespace "namespace"
  "The name prefix of namespace symbol.")
(/defmacro def-intern (name pos &optional symdoc formatdoc interndoc)
  (declare (doc-string 2) (indent defun))
  (:documentation
   (format "Define the functions to intern symbols with specific prefix or
suffix.\n
NAME is the namespace of the symbol.
POS is the position in symbol to put NAME.
 prefix (symbol) means using NAME as the prefix of the symbol.
 suffix (symbol) means using NAME as the suffix of the symbol.
The results are:
`%s' `%s' `%s' `%s'"
	   (/--intern* "%s-NAME" /--namespace-namespace)
	   (/--intern* "%s-NAME" /--namespace-format)
	   (/--intern* "%s-NAME" /--namespace-intern)
	   (/--intern* "%s-NAME*" /--namespace-intern)))
  (let* ((name (/--name name))
	 (str (format "%s-%s" /--namespace-namespace name))
	 (sym (/--intern* str))
	 (strfun (format "%s-%s" /--namespace-format name))
	 (symfun (/--intern* strfun))
	 )
    `(progn
       (/defconst* ,str ,name ,(format "The %s of %s symbol" pos name))
       (/defun* ,strfun (fmt &rest args)
	 ,(format "Format string with %s `%s'.\n
See `/--format' for arguments usage." pos name)
	 (apply #'format
		(format "%s-%s"
			,@(cond
			   ((eq pos 'prefix) (list sym '(/--name fmt)))
			   ((eq pos 'suffix) (list '(/--name fmt) sym))))
		args))
       (/defun* ,(format "%s-%s" /--namespace-intern name) (fmt &rest args)
	 ,(format "Intern the format string using `%s' with prefix `%s'.\n
See `/--format' for arguments usage." symfun /--namespace)
	 (/--intern (apply #',symfun fmt args)))
       (/defun* ,(format "%s-%s*" /--namespace-intern name) (fmt &rest args)
	 ,(format "Intern the format string using `%s' with prefix `%s%s'.\n
See `/--format' for arguments usage." symfun /--namespace /--namespace*)
	 (/--intern* (apply #',symfun fmt args))))))

;;; {{ custom symbol
(/def-intern custom prefix)
;; (/defmacro defcustom (name &optional init doc)
;;   (declare (doc-string 3) (indent defun))
;;   (:documentation
;;    (format "Define a variable which NAME is prefixed with `%s%s'.\n
;; See `/defmacro' for argument NAME. The rest arguments is the same as `defvar'."
;; 	   /--namespace /--namespace-custom))
;;   `(/defvar ,(/--format-custom name) ,init , doc))
(/defmacro defcustom (name &optional init doc)
  ""
  `(/defvar ,(/--fmt /--custom-format name) ,init ,doc))
;;; }}

(/defun* intern-format (fmt &rest args)
  "Intern the format string.\n\nSee `/--name' for arguments FMT."
  (intern (apply #'format (/--name fmt) args)))

(/defun path-directory (path &optional dir)
  "Return the absolute path in directory DIR.\n
PATH can be symbol, in that case the symbol name is used. See `/--name'.
If DIR is nil, `default-directory' is used.
The result path is obtained by following all the symbolic links."
  (file-truename (expand-file-name (/--name path) dir)))

(/defun path-user-directory (path)
  "Return the absolute path in `user-emacs-directory'.\nSee `/path-directory'"
  (/path-directory path user-emacs-directory))

;;; {{ directory symbol
(/def-intern directory suffix)
;;; }}

;;; {{ set up user lisp code load path
(/def-intern lisp prefix)
(/defun path-lisp-directory (path)
  (:documentation (format "Return the path in user lisp code directory.\n
It is searched in the `user-emacs-directory/%s/'" /--namespace-lisp))
  (/path-directory path (/path-user-directory /--namespace-lisp)))
(/defmacro def-lisp-directory (name &optional doc)
  (declare (doc-string 2) (indent defun))
  (:documentation "abcd")
  (let* ()
    `(progn
       (/defconst*))))
;;; }}


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
  :prefix (format "%s%s" /--namespace /--custom-name))

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
	 ))))

(defmacro /provide () "A wrapper around `/--provide'." `(/--provide))

(/def-lisp-directory init "Init file.")
(/def-lisp-directory config "Packages configuration.")
(/def-lisp-directory custom "User options.")
(/def-lisp-directory lib "Utility functions.")
(/def-lisp-directory meta "Utility macros.")
(/def-lisp-directory test "Test codes.")
(/def-lisp-directory tool "Batch utility tool.")


(/require-lib core)
;;(/--require 'lib 'core)

(defvar /--predictor-name "p" "The subname of predictor symbol.")

(defun /--intern-predictor (form &rest args)
  (declare (indent defun))
  (:documentation (format "Append %s to the user variable and intern it.
See `/intern'." /--predictor-name))
  (apply #'/--intern (format "%s-%s" (/--name form) /--predictor-name) args))

(defvar /--regexp-name "regexp" "The subname of regular expression variable.")

(defun /--intern-regexp (form &rest args)
  (declare (indent defun))
  (:documentation
   (format "Append %s to the custom format string and intern it.
See `/--intern-custom'." /--regexp-name))
  (apply #'/--intern-custom
	 (format "%s-%s" (/--name form) /--regexp-name) args))

(defun /--string (form)
  "Evaluate FORM adaptively until FORM is a string.
The function assumes that form will eventually turn into a string."
  (declare (indent defun))
  (while (not (stringp form)) (setq form (eval form))) form)

(defun /--list (form)
  "Evaluate FORM adaptively until FORM is a list.
The function assumes that form will eventually turn into a string."
  (declare (indent defun))
  (while (not (listp form)) (setq form (eval form))) form)

(defun /--quote (form)
  "Quote FORM is form is not a `quote' expression."
  (declare (indent defun))
  (and (or (symbolp form)     ;; Quote symbol.
	                      ;; Quote unquoted list.
	   (and (listp form) (not (eq 'quote (car form)))))
       (setq form `(quote ,form)))
  form)

;;; {{ Create variable with /custom prefix
(defmacro /def-custom-var (form &optional init doc depth)
  (declare (doc-string 3) (indent defun))
  (:documentation (format "Define a variable (defvar %s INIT DOC).
If DEPTH is non-nil, (/--value INIT DEPTH)." (/--intern-custom 'FORM)))
  `(defvar ,(/--intern-custom form)
     ,(if depth (/--value init depth) init) ,doc))

(defmacro /def-custom-const (form &optional init doc depth)
  (declare (doc-string 3) (indent defun))
  (:documentation (format "Define a constant (defconst %s INIT DOC).
If DEPTH is non-nil, (/--value INIT DEPTH)." (/--intern-custom 'FORM)))
  `(defconst ,(/--intern-custom form)
     ,(if depth (/--value init depth) init) ,doc))
;;; }}

;;; {{ sexp constructor.
(defun /--list-quote-all (list)
  "Return a list by quoting all elements in LIST."
  (mapcar (lambda (e) `(quote ,e)) list))

(defun /--list-quote-alternately (list &optional even)
  "Return a list by alternately quoting elements in LIST.
If EVEN is non-nil, start from first index, otherwise start from the
  second index."
  (declare (indent defun))
  (let* ((idx (if even 1 0)))
    (mapcar (lambda (e)
	      (if (/oddp (setq idx (1+ idx))) `(quote ,e) e))
	    list)))

(defun /--list-quote-odd (list)
  "Return a list by quoting all elements in odd index in LIST.
The start index is 1."
  (declare (indent defun))
  (/--list-quote-alternately list))

(defun /--list-quote-even (list)
  "Return a list by quoting all elements in even index in LIST.
The start index is 1."
  (declare (indent defun))
  (/--list-quote-alternately list 'even))

(defvar /--sexp-list (make-symbol "/--sexp-list---")
  "Store the list of sexp during the sexp construction")
(set /--sexp-list nil)

(defmacro /--sexp (&rest body)
  "Create a sexp from BODY.
The sexp is store in `/--sexp-list' and `/--sexp-list' is set to nil
  before evaluate BODY."
  (declare (indent defun))
  `(let* ((,/--sexp-list nil)) ,@body (nreverse ,/--sexp-list)))

(defmacro /--sexp-append-1 (expr)
  "Append EXPR to `/--sexp-list'."
  (declare (indent defun))
  `(push ,expr ,/--sexp-list))

(defmacro /--sexp-append-1-literally (expr)
  "Append EXPR to `/--sexp-list' without evaluating it."
  (declare (indent defun))
  `(push ',expr ,/--sexp-list))

(defmacro /--sexp-append (&rest exprs)
  "Append EXPRS to `/--sexp-list'."
  (declare (indent defun))
  `(setq ,/--sexp-list (apply #'/prepend ,/--sexp-list ,@exprs nil)))

(defmacro /--sexp-append-literally (&rest exprs)
  "Append EXPRS to `/--sexp-list' without evaluating all its subexpression."
  (declare (indent defun))
  `(/--sexp-append ,@(/--list-quote-all exprs)))

(defmacro /--sexp-append-literally-odd (&rest exprs)
  "Append EXPRS to `/--sexp-list' without evaluating its subexpression
in odd index.
The index starts from 1."
  `(/--sexp-append ,@(/--list-quote-odd exprs)))

;; progn
(defmacro /--sexp-progn (&rest body)
  "Create a `progn' form."
  (declare (indent defun))
  `(/--sexp (/--sexp-append-1 'progn) ,@body))

(defalias '/--sexp-exec #'/--sexp-append "Append SEXPS to `pron' list.")

(defmacro /--sexp-progn-exec (&rest body)
  "Create a `progn' form.
BODY is wrapped in `/--sexp-exec' form so other constructor has no effect."
  (declare (indent defun))
  `(/--sexp-progn (/--sexp-exec ,@body)))

;; cond
(defmacro /--sexp-cond (&rest body)
  "Create a `cond' form."
  (declare (indent defun))
  `(/--sexp (/--sexp-append-1 'cond) ,@body))

(defmacro /--sexp-cond-case (&rest body)
  "Create a `cond' form.
BODY is wrapped in `/--sexp-exec' form so other constructor has no
  effect. BODY should be a list of form (csexp forms)."
  (declare (indent defun))
  `(/--sexp-cond (/--sexp-exec ,@body)))

(defmacro /--sexp-case (csexp &rest body)
  "Create a `case' form."
  (declare (indent defun))
  `(/--sexp-append (,csexp ,@body)))

(defmacro /--sexp-setq (&rest body)
  "Create a `setq' form"
  (declare (indent defun))
  `(/--sexp (/--sexp-append-1 'setq) ,@body))

(defalias '/--sexp-pair #'/--sexp-append
  "Create `key value' items in `setq' form.")

;;; }}

;; eval-after-load
(defmacro /eval-after-load (package &rest body)
  "Evaluate BODY after load PACKAGE.
The expressions in BODY are quoted automatically, which is unlike tool
tool macros such as `/--sexp-pron-exec', `/--sexp-exec', etc, because
this macro is intended for the top level use, not for building macros."
  (declare (indent defun))
  (/--sexp
    (/--sexp-append 'eval-after-load (/--quote package)
      `(quote ,(/--sexp-progn (dolist (form body) (/--sexp-append-1 form)))))))

;; swap
(defmacro /swap (var1 var2)
  "Swap two variable VAR1 VAR2."
  (declare (indent defun))
  (let* ((tmp (make-symbol "tmp")))
    `(setq ,tmp ,var1 ,var1 ,var2 ,var2 ,tmp)))

;; double keys event
(defvar /--double-keys-event-interval 0.2 ;(/ double-click-time 1000.0)
  "Interval seconds to distinguish normal event and double keys event.")

(defmacro /def-double-keys-event-command
    (name cmd cmd2 doc &optional interval)
  "Define a double keys event command.
A double keys event command is a command that when invoked, it will continue
    read events in a time INTERVAL. If the events is the same as the events that
    invokes this command, it will execute the double events command CMD2,
    otherwise it will execute normal command CMD."
  (declare (doc-string 4) (indent defun))
  (or interval (setq interval /--double-keys-event-interval))
  `(defun ,(/--intern name) ()
     ,doc
     (interactive)
     (let* ((keys (this-command-keys-vector))
	    (events (/wait-for-events ,interval (length keys))))
       ;(message "keys: %s %s" keys (key-description keys))
       ;(message "even: %s %s" events (key-description events))
       (if (equal keys events)
	   (progn ,@cmd2)
	 ,@cmd (/put-back-events events)))))

(/provide)
;;; meta/core.el ends here
