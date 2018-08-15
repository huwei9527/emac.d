;;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

;;; Commentary:

;;; Code:

(/defmacro* format-get-format (symbol)
  "Get the `/--format' of SYMBOL.
SYMBOL is a `/--symbol' argument."
  `(get (/--symbol ,symbol) '/--format-format))

(/defmacro* format-set-format (symbol format)
  "Store the `/--format FORMAT' to SYMBOL."
  `(put (/--symbol ,symbol) '/--format-format ,format))

(/defmacro* format-get-function (symbol)
  "Get the `/--format-function' of SYMBOL"
  `(get (/--symbol ,symbol) '/--format-function))

(/defmacro* format-set-function (symbol function)
  "Store the `/--format-function FUNCTION' to SYMBOL."
  `(put (/--symbol ,symbol) '/--format-function ,function))

(/cl-defmacro* defformat (name &key (format nil formatp) (func nil funcp))
  "Define a `/--format symbol'.
NAME is a `/--symbol' argument.
FORMAT is a `/--format'.
FUNC is a `/--format-format'."
  (declare (indent defun))
  `(progn
     ,(and formatp `(/--format-set-format ',name ,format))
     ,(and funcp `(/--format-set-function ',name ,func))))

(/require-lib core)

(/--defformat - :format "-")
(/--defformat custom :format "custom")
(/--defformat directory :format "directory")
(/--defformat lisp :format "lisp")
(/--defformat lispdir :format '(lisp %s directory))
(/--defformat require :format "require")
(/--defformat file-name :format "file-name")
(/--defformat pred :format "p")
(/--defformat regexp :format "regexp")
(/--defformat namespace :format /--namespace)
(/--defformat namespace* :format /--namespace*)

;; Define `/custom' variable
(/defmacro defcustom (name &optional init doc)
  "`/custom' variable for customization.
NAME is a `custom /--format /--name' argument interned in `/--namespace'.
The rest arguments is the same as `defvar'.
For example,
  (/defcustom abcd ...) => (defvar /custom-abcd ...)"
  (declare (indent defun))
  `(/defvar ((custom - %s) ,@(/--list name)) ,init ,doc))

;; Define `/regexp' variable
(/defmacro defregexp (name regexp doc &optional quote)
  "Define `/custom' variable for FORM regexp.
The variable is of `custom regexp /--format'.
NAME is a `/--name' argument.
REGEXP is a `/regexp' string.
DOC is the doc-string of the `custom' variable."
  (declare (indent defun))
  `(/defcustom ((%s - regexp) ,@(/--list name))
	       ,(if quote (/regexp-quote regexp) regexp)
	       ,(format "The regular expression for `%s%s'.%s"
			/--namespace (/--name name)
			(if doc (format "\n%s" doc) ""))))

;; Define `/pred' function
(/defmacro defpred (name &optional doc)
  "Define a `/pred' function.
The name of `pred' function is of `pred /--format'.
It accept a `/--name' argument and use `NAME /regexp' to do the prediction.
NAME is a `/--name' argument.
DOC is the doc-string of the `/pred' function.
The `/pred' use regexp `%s' to check."
  (declare (indent defun))
  (let* ((name (/--name name))
	 (regexp (/--intern '(custom - %s - regexp) name)))
    `(/defun ((%s - pred) ,@(/--list name)) (string)
       ,(format "`/pred' function for `%s%s'.
Make a regexp search with regexp `%s'.
STRING is a `/--name' argument.%s"
		/--namespace name regexp (if doc (format  "\n%S" doc) ""))
       (string-match-p ,regexp (/--name string)))))

;;; {{ sexp constructor.
(/defvar* sexp--form-symbol (make-symbol "/--sexp--list")
  "The uninterned symbol to store the form list.")

(/defmacro* sexp (&rest body)
  "Construct a list as sexp.
The list is stored in the local variable `/--sexp--form-symbol'.
Use `/--sexp' macros to construct the list."
  (declare (indent defun))
  `(let* ((,/--sexp--form-symbol nil)) ,@body (nreverse ,/--sexp--form-symbol)))

(/defmacro* sexp-add (&rest body)
  "Add expressions to the end of `/--sexp--form-symbol'.
Each expression is evaluated."
  (declare (indent defun))
  `(setq ,/--sexp--form-symbol (funcall #'/push ,/--sexp--form-symbol ,@body)))

(/defmacro* sexp-quote-add (&rest body)
  "Add expressions to the end of `/--sexp--form-symbol'.
Each expression is quoted before it is added."
  (declare (indent defun))
  `(/--sexp-add ,@(/seq-quote body)))

(/defmacro* sexp-quote-add-even (&rest body)
  "Add expressions to the end of `/--sexp--form-symbol'.
Each expression in even index is quoted before it is added.
The index starts from 0."
  (declare (indent defun))
  `(/--sexp-add ,@(/seq-quote-even body)))

(/defmacro* sexp-append (&rest body)
  "Append expression sequence to the end of `/--sexp--form-symbol'.
This is similar `append'."
  (declare (indent defun))
  `(setq ,/--sexp--form-symbol
	 (nreverse (append (nreverse ,/--sexp--form-symbol) ,@body))))

;; progn
(/defmacro* sexp-progn (&rest body)
  "Construct a `progn' form.
Use `/--sexp' macro to construct the `progn' form."
  (declare (indent defun))
  `(/--sexp (/--sexp-add 'progn) ,@body))

(/defalias* sexp-exec #'/--sexp-add
  "Add expressions to the end of `progn' form.
Each expression is evaluated.")

(/defmacro* sexp-progn-exec (&rest body)
  "Construct a `progn' form.
The expressions are add to the end of the `progn' form.
Equivalent to (/--sexp-progn (/--sexp-exec ,@body)).
The expression in BODY is evaluated."
  (declare (indent defun))
  `(/--sexp-progn (/--sexp-exec ,@body)))

;; cond
(/defmacro* sexp-cond (&rest body)
  "Construct a `cond' form.
Use `/--sexp' macro to construct the `cond' form."
  (declare (indent defun))
  `(/--sexp (/--sexp-add 'cond) ,@body))

(/defmacro* sexp-cond-case (&rest body)
  "Construct a `cond' form.
The expressions are add to the end of the `cond' form.
Equivalent to (/--sexp-cond (/--sexp-exec ,@body)).
To make a valid `cond' form, the expressions should be list of `cond' 
case form `(condition form1 form2 ...)'."
  (declare (indent defun))
  `(/--sexp-cond (/--sexp-exec ,@body)))

(/defmacro* sexp-case (cond &rest body)
  "Add a `cond' case form to `cond' form.
COND is the condition of the case form.
BODY is the body expressions of the case form.
The expressions in COND and BODY are evaluated."
  (declare (indent defun))
  `(/--sexp-add (/--sexp (/--sexp-add ,cond ,@body))))

(/defmacro* sexp-setq (&rest body)
  "Construct a `setq' form.
Use `/--sexp' macros to construct the `setq' form."
  (declare (indent defun))
  `(/--sexp (/--sexp-add 'setq) ,@body))

(/defalias* sexp-pair #'/--sexp-quote-add-even
  "Add a `setq' pair to `setq' form.
The `setq' pair is a `symbol value' pair in which `symbol' is not evaluated
and `value' is evaluated. So the expression in even index is quoted to prevent
`symbol' evaluated.")

(/defmacro* sexp-setq-pair (&rest body)
  "Construct a `setq' form.
The expressions is add to the end of the `setq' form and the expression
in even index is quoted.
Equivalent to (/--sexp-seq (/--sexp-pair ,@body))."
  `(/--sexp-setq (/--sexp-pair ,@body)))
;; ;;; }}

(/defmacro require (feature &rest modules)
  "Load `/--feature' FEATURE in multiple `/--module' MODULES."
  (/--sexp-progn
    (dolist (module modules)
      (/--sexp-exec `(,(/--intern '(require - %s) module) ,feature)))))

;; eval-after-load
(/defmacro eval-after-load (package &rest body)
  "Evaluate BODY after load PACKAGE.
The expression in BODY is not evaluated."
  (declare (indent defun))
  (/--sexp (/--sexp-add 'eval-after-load (/--quote package)
			(macroexp-quote
			 (/--sexp-progn (/--sexp-append body))))))

;; swap
(/defmacro swap (var1 var2)
  "Swap two variable VAR1 VAR2."
  (declare (indent defun))
  (let* ((tmp (make-symbol "tmp")))
    `(setq ,tmp ,var1 ,var1 ,var2 ,var2 ,tmp)))

;; double keys event
(/defvar* double-keys-event-interval 0.2 ;(/ double-click-time 1000.0)
  "Interval seconds to distinguish normal event and double keys event.")

(/defmacro def-double-keys-event-command
    (name cmd cmd2 doc &optional interval)
  "Define a double keys event command.
A double keys event command is a command that when invoked, it will continue
    read events in a time INTERVAL. If the events is the same as the events that
    invokes this command, it will execute the double events command CMD2,
    otherwise it will execute normal command CMD."
  (declare (doc-string 4) (indent defun))
  (or interval (setq interval /--double-keys-event-interval))
  `(/defun ,name ()
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
