;;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

;;; Commentary:

;;; Code:

(/require-lib core)

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
