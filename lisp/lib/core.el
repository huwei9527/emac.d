;;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

;;; Commentary:

;;; Code:

(require 'cl-lib)

;;; {{ Define argument type

(/defun* value (form pred)
  ""
  (while (not (funcall pred form)) (setq form (eval form)))
  form)

;; Define /--name.
(/defun* name (form)
  "Return the `/--name' of FORM.
For `/--name' argument,
  string   => FORM
  number   => number string
  symbol   => `symbol-name'
  list     => `/--format' string
  other    => error"
  (cond
   ((stringp form) form)
   ((numberp form) (format "%s" form))
   ((symbolp form) (symbol-name form))
   ((listp form) (apply #'/--format form))
   (t (/--error-unsupported-type form))))

;; Define /--quote argument
(/defun* quotep (form)
  "Return non-nil if FORM is a `quote' form.
A `/--quote' form is a list of which the first element is the symbol
`quote', `function' or `cl-function'."
  (memq (car-safe form) '(quote function cl-function)))

(/defun* quote (form)
  "Return `/--quote' form of FORM.
If FORM is not a `/--quote' form, return `quote' form of it."
  (if (/--quotep form) form (macroexp-quote form)))

(/defun* unquote (form)
  "Return form of no `/--quote'.
If FORM is a `/-quote' form, return the `/--quote'd content. Continue this
process until the result is not a `/-quote' form."
  (while (/--quotep form) (setq form (cadr form))) form)

;; Define /--list argument
(/defun* list (form)
  "Return `/--list' form of FORM.
If FORM is not a list form, return a list containing it."
  (if (listp form) form (list form)))

;; Define /--symbol argument.
(/defun* symbol (symbol)
  "Return a `/--symbol' of SYMBOL.
The `/--symbol' is to intern the `/--name' of SYMBOL."
  (condition-case nil
      (intern (/--name symbol))
    (error (/--error-unsupported-type symbol))))

;; Define /--file-name argument.
(/defun* file-name (path)
  "Return `/--file-name' of PATH.
For a `/--file-name' argument,
  string - path string, return the file name without directory.
           If it is a directory, return the name of the directory
           in the last level.
  nil    - Return the current buffer name.
  buffer - Return the buffer name of PATH.
  other  - error"
  (cond
   ((stringp path) (file-name-nondirectory (directory-file-name path)))
   ((null path) (buffer-name))
   ((bufferp path) (buffer-name path))
   (t (/--error-unsupported-type path))))

;; Define /--buffer-file-name argument.
(/defun* buffer-file-name (&rest buf)
  "Return the `/--buffer-file-name' of the buffer BUF.
If BUF is visiting a file, return the `buffer-file-name'. 
Otherwise return the `buffer-name'

If BUF is nil, use the current buffer."
  (or buf (setq buf (current-buffer)))
  (if (buffer-file-name buf) (buffer-file-name buf) (buffer-name buf)))

;; Define /--function argument.
(/defun* function (func &optional default)
  "Return a `/--function' of FUNC.
The `/--function' is a list (def ,@args) where
  def  - definition of the function which can be the function argument 
         of `funcall' and `apply'.
  args - additional arguments

FUNC and DEFALT are `/--function' arguments.

If FUNC is a function, return (FUNC)
If FUNC is list, return itself
If FUNC is nil, return DEFAULT
If DEFAULT is nil, return (identity)"
  (if func (if (functionp func) (list func)
	     (if (listp func) func (/--error-unsupported-type func)))
    (if default (/--function default) (list #'identity))))

(/defun* funcall (func &rest args)
  "Call FUNC with ARGS.
This is similar to `funcall' except FUNC is a `/--function' argument.
So the additional arguments in FUNC is appended to the end of ARGS.

For a `/--function' of the form (def ,@funargs), the process is like 
`(funcall def ,@args ,@funargs)'.

For example,
  (/--funcall '(message 1 2 3 4) \"%s%s%s%s%s%s\" 0 \"X\") => \"0X1234\"
  (/--funcall '(< 5 6 7) 4 3 2) => nil    (< 4 3 2 5 6 7)"
  (setq func (/--function func))
  (apply (car func) (append args (cdr func))))

(/defun* apply (func &rest args)
  "Call FUNC with remaining ARGS, using last argument as list of args.
This is similar to `apply' except FUNC is a `/--function' argument.
So the additional arguments in FUNC is appended to the end of ARGS.

For a `/--function' of the form (def ,@funargs), the process is like
`(apply def ,@(butlast args) (append (last args) funcargs))'

For example,
  (/--apply '(message 1 2 3 4) \"%s%s%s%s%s%s\" 0 '(\"X\")) => \"0X1234\"
  (/--apply '(< 5 6 7) 4 '(3 2)) => nil    (< 4 3 2 5 6 7)"
  (setq func (/--function func))
  ;; FIXME: Change two parse to one parse of ARGS
  (apply (car func) (append (butlast args) (car (last args)) (cdr func))))

;; Define /--format-format
(/cl-defgeneric* format--parse (format)
  "Return the value of `/--format-format'.

The `/--format-format' FORMAT is a constant `/--format-format' if it is
not a `/--format-format symbol'.

The return value is a list (fmt value).
  fmt   - the `/--format-format symbol'
  value - the string of constant `/--format-format'

If FORMAT is a `/--format-format symbol', `value' is nil.
If FORMAT is a constant `/--format-format', `fmt' is nil."
  (:method ((format string))
	   "`/--format-format' is string.
This is constant `/--format-format'. Return `(nil FORMAT)'."
	   (cl-values nil format))
  (:method ((format number))
	   "`/--format-format' is number.
This is constant `/--format-format'. Return `(nil number-string)'."
	   (/--format--parse (format "%s" format)))
  (:method ((format symbol))
	   "`/--format-format' is symbol.

`/--format symbol' is a symbol containing non-nil `/--format-format' property.
Otherwise the symbol is a normal symbol and the `symbol-name' is used.

The value of the `/--format-format' property can be,
  string - `string /--format-format'
  number - `number /--format-format'
  symbol - `symbol /--format-format'
  list   - list of `/--format-format'
             - `list /--format'
             - list of `list /--format'
  
`/--format-format symbol' is a symbol the value of the property of which is
  - a `/--format-format symbol'
  - a `/--format-format list'
  - a list of `/--format-format list'
The basic `/--format-format symbol' is the symbol %s.

In other word, if the content of the property contains at least one 
constant `/--format-format', the symbol is constant `/--format-format'.

If FORMAT is not `/--format symbol', return (nil symbol-name)
If FORMAT is `/--format-format symbol', return (format nil)
If FORMAT is constant `/--format-format', return (nil /--format-string)"
	   (if (eq format '%s) (cl-values '%s)  ; basic /--format-format symbol
	     (let* ((content (/--format-get-format format)))
	       (if content
		   ;; /--format symbol
		   (if (listp content)
		       ;; The content is list /--format
		       (let* (str fmtp value)
			 (if (listp (car content))
			     ;; list of list /--formats
			     ;; compute from left to right
			     (let* (xfmtp xvalue)
			       (setq fmtp t)
			       (dolist (f content)
				 (cl-multiple-value-setq (xfmtp xvalue)
				   (/--format--parse-list f))
				 (if xfmtp
				     (and str (setq str (/--format f str)))
				   (setq str xvalue fmt nil))))
			   ;; list /--format
			   (cl-multiple-value-setq (fmtp value)
			     (/--format--parse-list content)))
			 (if fmtp (cl-values format) (cl-values nil value)))
		     ;; The content is constant /--format
		     (/--format--parse content))
		 ;; Non /--format symbol
		 (/--format--parse (symbol-name format))))))
  (:method ((format list))
	   "`/--format-format' is `/--format-function'.
This is constant `/--format-format'. Return `(nil FORMAT)'"
	   (cl-values nil format))
  (:method (format)
	   "Fall through type."
	   (error "Invalid parse format %s" format)))

;; Define `list /--format-format'
(/defun* format--parse-list (format)
  "Parse the `list /--format'.
The element of `list /--format' is called `/--format-format'.
Parse each `/--format-format' of `list /--format' to a list 
`(fmt prefix suffix function)', in which
  fmt      - the `/--format-format symbol'
  prefix   - the concatenation of strings before `fmt'
  suffix   - the concatenation of strings after `fmt'
  function - the list of `/--format-function'

`/--format-format list' is a `list /--format' contains exactly one 
`/--format-format symbol'.

`list /--format' is a constant `/--format' if it contains no
`/--format-format symbol'.

For a constant `/--format', the `/--format-format symbol' is nil so we can't 
distinguish prefix and suffix. In this case, the return list is 
`(nil prefix nil function)' where `prefix' is the constant `/--format' string."
  (let* (fmt prefix suffix function xfmt xvalue)
    (dolist (f format)
      (if (listp f) (push f function)
	(cl-multiple-value-setq (xfmt xvalue) (/--format--parse f))
	(if xfmt (if fmt (error "Multiple format %s %s" fmt xfmt)
		   (setq fmt xfmt))
	  (push xvalue (if fmt suffix prefix)))))
    (cl-values fmt
	       (if prefix (apply #'concat (nreverse prefix)) "")
	       (if suffix (apply #'concat (nreverse suffix)) "")
	       (nreverse function))))

;; Define /--format
(/cl-defgeneric* format (format &rest args)
  "Return the `/--format' string.
The `/--format' is an extention of format-string of `format'."
  (:method ((format string) &rest args)
	   "Return the `/--format' string of `string /--format'.
This is the same as `format'. Use `format' to get the `/--format' string."
	   (apply #'format format args))
  (:method ((format symbol) &rest args)
	   "Return the `/--format' string of `symbol /--format'.
The `symbol-name' of FORMAT is used. Then use `format' to get the 
`/--format' string."
	   (apply #'/--format (symbol-name format) args))
  (:method ((format number) &rest args)
	   "Return the `/--format' string of `number /--format'.
The number string is used. Then use `format' to get the `/--format' string."
	   (apply #'/--format (format "%s" format) args))
  (:method ((format list) &rest args)
	   "Return the `/--format' string of `list /--format'.
The `list /--format' is a list of string, number, symbol and 
`/--format-function'.
The string, number and symbol can be parsed into strings.

`/--format-function symbol' is a symbol with non-nil `/--format-function'
property. The content of the property is a normal function.

`/--format-function' is a list of the form (def ,@args):
  def  - definition of function. This is similar to `/--function'.
         The `def' can be:
           - `/--function symbol'
           - function symbol
           - function
  args - additional arguments

A constant `/--format' is a `/--format' that can return the `/--format'
string independent of the rest arguments.

FORMAT is parsed with `/--format--parse-list' to (fmt prefix suffix function),
then do the following process
  1) Get the `/--format' string of the rest argument ARGS
  2) Feed the result string to the first `/--function' in `function' with its
     additional arguments. Then feed the return string to the second
     `/--function' in `function'. Then to the third, fourth ... from left to
     right in the same adaptive way.
  3) Format the result string in step 2) with the `/--format-format symbol'
     `fmt'.
     a) If `fmt' is nil, return a empty string.
     b) If `fmt' is %s (a symbol), return the identical string.
     c) If the value of `/--format-format symbol' is
          `list /--format' - return the `/--format' string with the result
                             string in 2) as argument
          list of `list /--format' - return the `/--format' string by applying
                                     these `/--format' to the result string
                                     from left to right in an adaptive way
  4) Return the string by adding `prefix' and `suffix' to the 
     `/--format-format' string in 3).

Note:
If `fmt' is nil, the `list /--format' is a constant `/--format'. In this case,
`/--function' has no effect. Actually `suffix' is nil and the constant 
`/--format' string is store in `prefix'. Return the string `prefix'.

So `/--format-format symbol' can be treated as place holder for the `/--format'
string of rest arguments. `/--function' can be treated as pre-processor to the
`/--format' string of rest arguments.

The `/--format' process can be simply explained as concatenation of the result
string of each `/--format-format' in the `list /--format'.

* Example
  (put 'fmtM '/--format-format \"M\")
  (put 'fmtM '/--format-function (lambda (s) (format \"M%sM\" s)))
  (put 'fmtN '/--format-format '(\"N\" %s \"N\"))
  (put 'fmtP '/--format-format '((fmtN) (\"P\" %s \"P\")))
  (/--format '(\"A\" 1 fmtM))                          => \"A1M\"
  (/--format '(fmtM %s (fmtM)) \"%s%s\" \"XX\" \"YY\") => \"MMXXYYM\"
  (/--format '(\"A\" fmtN \"B\") \"XXX\")              => \"ANXXXNB\"
  (/--format '(fmtP) \"XXX\")                          => \"PNXXXNP\""
	   (let* (fmt prefix suffix function)
	     (cl-multiple-value-setq
		 (fmt prefix suffix function) (/--format--parse-list format))
	     (if fmt
		 (let* ((str (if args (apply #'/--format args) "")) fun)
		   ;; Apply /--format-function to format string
		   (dolist (f function)
		     (if (symbolp (car f))
			 (progn
			   (setq fun (/--format-get-function (car f)))
			   (or fun (setq fun (car f))))
		       (setq fun (car f)))
		     (setq str (apply fun str (cdr f))))
		   ;; /--format the string using `fmt'
		   (or (eq fmt '%s)
		       (let* ((fmt (/--format-get-format fmt)))
			 (if (listp (car fmt))
			     (dolist (f fmt) (setq str (/--format f str)))
			   (setq str (/--format fmt str)))))
		   (format "%s%s%s" prefix str suffix))
	       ;; `format' is not a /--format format.
	       (format "%s" prefix))))
  (:method (format &rest _args)
	   "Fall through type"
	   (error "Invalid format %s" format)))

(/defun intern (format &rest args)
  "Intern the `/--format' string."
  (intern (apply #'/--format format args)))
;;; }}

(/defun true (&rest _ingore)
  "Ignore any argument and return t."
  (interactive)
  t)

(/defun false (&rest _ignore)
  "Ignore any argument and return nil."
  (interactive)
  nil)

(/defun mute-call (fun &rest args)
  "Call FUN with ARGS with no message."
  (let* ((inhibit-message t))
    (apply fun args)))

(/defsubst neq (obj1 obj2)
  "Return t if OBJ1 is not `eq' OBJ2."
  (not (eq obj1 obj2)))

(/defsubst nequal (obj1 obj2)
  "Return t if OBJ1 is not `equal' OBJ2."
  (not (equal obj1 obj2)))

;; Define `/regexp'
(/cl-defgeneric regexp-quote (expr &rest args)
  "Return the `/regexp' of EXPR.
The `/regexp' is construct according to the following rules:
  \(  => \\\\(  group start
  \)  => \\\\)  group end
  |  => \\\\|  the group delimeter
  .  => \\\\.  dot character
  *  => \\\\*  aterisk character
  `  => \\\\`  begin
  '  => \\\\'  end

If there is only one argument, return the `/regexp' of the argument.
If more than one, return a list of `/regexp' for each argument."
  (:method ((string string) &rest args)
	   "The first element is a string."
	   (if args
	       (let* ((quote-string (/regexp-quote string))
		      (quote-list (apply #'/regexp-quote args)))
		 (if (> (length args) 1) (cons quote-string quote-list)
		   (list quote-string quote-list)))
	     (replace-regexp-in-string "[()|.*`']" "\\\\\\&" string)))
  (:method ((symbol symbol) &rest args)
	   "The first element is a symbol.
Use the `symbol-name'."
	   (apply #'/regexp-quote (symbol-name symbol) args))
  (:method ((number number) &rest args)
	   "The first element is a number.
User the printed representation."
	   (apply #'/regexp-quote (format "%s" number) args))
  (:method ((list list) &rest args)
	   "The first element is a list.
Recursively transform the list."
	   (if args (cons (apply #'/regexp-quote list)
			  (apply #'/regexp-quote args))
	     (apply #'/regexp-quote list)))
  (:method (expr &rest args)
	   "Fall through"
	   (/--error-unsupported-type expr)))

;;; {{ Sequence tools
(/cl-defgeneric seq-map-alternate (seq func1 &optional func2)
  "Apply FUNC1 and FUNC2 to each element alternatively.
FUNC1 and FUNC2 are /--function parameters."
  (:method ((list list) func1 &optional func2)
	   "Sequence is a list."
	   (let* ((idx 0))
	     (setq func1 (/--function func1) func2 (/--function func2))
	     (mapcar (lambda (e)
		       (if (cl-oddp (cl-incf idx)) (/--funcall func1 e)
			 (/--funcall func2 e)))
		     list)))
  (:method (seq _func1 &optional _func2)
	   "Fall through type."
	   (/--error-unsupported-type seq)))

(/defun prepend (&rest args)
  "Concatenate all the arguments in the reverse order.
Equivalent to `(append ,@(nreverse args))"
  (apply #'append (nreverse args)))

(/cl-defgeneric push (seq &rest args)
  "Push the rest arguments to sequence SEQ.
The last argument is the first element in the result sequence.
The type of result sequence is the same as SEQ.
Return the result sequence."
  (:method ((list list) &rest args)
	   "Sequence LIST is list."
	   (append (nreverse args) list))
  (:method (seq &rest _args)
	   "Fall through."
	   (/--error-unsupported-type seq)))

(/cl-defgeneric seq-quote (seq)
  "Return the sequence with all elements quoted.
The return sequence has the same type as SEQ."
  (:method ((list list))
	   "Sequence LIST is a list."
	   (mapcar #'macroexp-quote list))
  (:method (seq)
	   "Fall through type."
	   (/--error-unsupported-type seq)))

(/cl-defgeneric seq-pick-quote (seq pick)
  "Return the sequence with elements picked by PICK quoted.
PICK is an function (PICK ELT INDEX) accept two arguments. The first
is the element in the sequence. The second is the element index in the
sequence. Index start from 0.
If PICK return non-nil, quote the element, otherwise leave it unchanged.
The return sequence has the same type as SEQ."
  (:method ((list list) pick)
	   "Sequence LIST is a list."
	   (let* ((idx -1))
	     (mapcar (lambda (e)
	     	       (if (funcall pick e (cl-incf idx)) (macroexp-quote e) e))
	     	     list)))
  (:method (seq)
	   "Fall through type."
	   (/--error-unsupported-type seq)))

(/defun seq-quote-even (seq)
  "Return the sequence with elements in even index quoted.
Index start from 0.
The return sequence has the same type as SEQ."
  (/seq-pick-quote seq (lambda (_e i) (cl-evenp i))))

(/defun seq-quote-odd (seq)
  "Return the sequence with elements in odd index quoted.
Index start from 0.
The return sequence has the same type as SEQ."
  (/seq-pick-quote seq (lambda (_e i) (cl-oddp i))))
;;; }}

(/defsubst time-since (last)
  "Seconds between current time and LAST."
  (time-to-seconds (time-since last)))

(/defalias evenp 'cl-evenp)
(/defalias oddp 'cl-oddp)

;;; {{ character prediction
;; Define /--character
(/defun* character (c)
  "Return a `/-character' of C.
For a `/--character' argument C, if it is
  character - return itself
  symbol    - return the first character of the `symbol-name'
  string    - return the first character of the string
  other     - nil"
  (if c (cond ((characterp c) c)
	      ((symbolp c) (elt (symbol-name c) 0))
	      ((stringp c) (elt c 0))
	      (t nil))
    nil))

(/defun char-space-p (c)
  "Return t if C is a space character."
  (eq ?\s (char-syntax (/--character c))))

(/defun char-escape-p (c)
  "Return t if C is a escape character."
  (eq ?\\ (char-syntax (/--character c))))

(/defun char-word-p (c)
  "Return t if C is a word constituent."
  (eq ?w (char-syntax (/--character c))))

(/defun char-symbol-p (c)
  "Return t if C is a symbol constituent.
This is the extra character which constitute word besides those the
  type of which is Word."
  (eq ?_ (char-syntax (/--character c))))

(/defun char-at-point-word-p ()
  "Return non-nil if the character just before the point is a word.
See `preceding-char', `/char-word-p', `/char-symbol-p'."
  (and (not (eq (point) (line-beginning-position)))
       (let* ((c (preceding-char)))
	 (or (/char-word-p c)
	     (/char-symbol-p c)))))
;;; }}

;;; {{ shell-command
;; FIXME: use emacs asynchronous process to feed input.
(/defun shell-command-to-string (cmd &optional input)
  "Execute CMD.
If INPUT is non-nil, it is feed to the standard input."
  (if input
      (shell-command-to-string
       (format "echo %s | %s" (shell-quote-argument input) cmd))
    (shell-command-to-string cmd)))
;;; }}

(/defun wait-for-events (&optional sec n)
  "Wait for N (default 1) events for at most SEC milliseconds.
If SEC is nil, wait infinitely."
  (or n (setq n 1))
  (let* ((time (if sec (time-add sec nil) nil)) e events)
    (catch 'break-tag
      (dotimes (i n)
	;(push (read-event nil nil sec) events)
	(and (setq e (read-event nil nil sec)) (push e events))
	;(message "wait: %s %s" events (key-description events))
	(and time (time-less-p time nil) (throw 'break-tag nil))
	(setq sec (float-time (time-subtract time nil)))))
    (vconcat (nreverse events))
    ))

(/defun put-back-events (&optional events)
  "Put EVENTS back to `unread-command-events'."
  (or events (setq events (this-command-keys-vector)))
  (setq unread-command-events (append unread-command-events events nil)))

(/provide)
;;; lib/core.el ends here
