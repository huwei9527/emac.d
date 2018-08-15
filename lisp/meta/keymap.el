;;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

;;; Commentary:

;;; Code:

(/require-lib core)
(require 'cl-lib)

(/--defformat define-keys :format "define-keys")
(/--defformat mode :format "mode")
(/--defformat map :format "map")

;; Define `/--kbd'
(/defun* kbd (keys &rest args)
  "Return `/--kbd' of KEYS.
The `/--kbd' argument is a vector, if KEYS is
  vector   - return itself.
  string   - return vector of the `kbd'
  symbol   - the `symbol-name' is used
  interger - treated as character make it a vector
  /--quote - the `/--unquote' value is used
  function - the return value of the function is used
  other    - error
If ARG is non-nil, `vconcat' the result of each argument."
  (cond
   ((stringp keys) (apply #'/--kbd (string-to-vector (kbd keys)) args))
   ((symbolp keys) (apply #'/--kbd (symbol-name keys) args))
   ((vectorp keys) (if args (vconcat keys (apply #'/--kbd args)) keys))
   ((integerp keys) (apply #'/--kbd (vector keys) args))
   ((/--quotep keys) (apply #'/--kbd (/--unquote keys) args))
   ((functionp keys) (apply #'/--kbd (funcall keys) args))
   (t (/--error-unsupported-type keys))))

;; Define `/--keys'.
(/defun* keys (keys &optional prefix)
  "Return the `/--keys' of (PREFIX KEYS).
KEYS and PREFIX are `/--kbd' argument.
If PREFIX is nil, return the `/--kbd' of KEYS"
  (if prefix (vconcat (/--kbd prefix) (/--kbd keys)) (/--kbd keys)))

;; Define `/--keymap'
(/defun* keymap (keymap)
  "Return `/--keymap' of KEYMAP.
For the `/--keymap' argument,
  keymap - If it is a symbol, return `symbol-function', otherwise itself.
  symbol - If the `symbol-value' is keymap, return it.
           If `symbol-name' is of `map /--format', use the `map /--format'
           symbol.
           otherwise raise an error.
  list   - `/--format', use the symbol of the `/--format'."
  (cond
   ((keymapp keymap) (if (symbolp keymap) (symbol-function keymap) keymap))
   ((symbolp keymap)
    (if (and (boundp keymap) (keymapp (symbol-value keymap)))
	(symbol-value keymap)
      (if (string-suffix-p (/--format-get-format 'map) (symbol-name keymap))
	  (error "Invalid keymap %s" keymap)
	(/--keymap `((%s - map) ,keymap)))))
   ((listp keymap) (/--keymap (apply #'/intern keymap)))
   (t (/--error-unsupported-type keymap))))

(/defun* lookup-key (keymap keys &optional prefix)
  "Lookup `/--keys' (PREFIX KEYS) in `/--keymap' KEYMAP.
(KEYS PREFIX) are `/--keys' argument
KEYMAP is `/--keymap' argument.
If the `/--kbd' of `/--keys' is too long, return nil.
This is different from `lookup-key' in that case it return a number."
  (setq keymap (lookup-key (/--keymap keymap) (/--keys keys prefix)))
  ;; `lookup-key' returns a number if the KEYS is too long. Make it nil.
  (if (numberp keymap) nil keymap))

;; Define /--key-definition argument.
(/defun* key-definition (def)
  "Return `/--key-definition' of DEF.
For a `/--key-definition' argument,
  symbol - return the function quote of the symbol `(function DEF)'
  string - return `/--keys' of DEF
  keymap - return `(quote DEF)'
  list   - If the first element is
            string  - (STRING . DEFN)
            keymap  - (MAP    . CHAR)
            closure - a closure
           return `(quote DEF)', otherwise return itself
  other - return itself

In other word, quote the DEF if needed."
  (cond ((symbolp def) `(function ,def))
	((stringp def) (/--keys def))
	((keymapp def) (macroexp-quote def))
	((listp def) (let* ((first (car def)))
		       (if (or (stringp first)     ; (STRING . DEFN)
			       (keymapp first)     ; (MAP    . CHAR)
			       (eq 'closure first) ; (CLOSURE)
			       ;(eq 'lambda first) lambda is eval to itself.
			       )
			   (macroexp-quote def)
			 def)))
	(t def)))

;; Define `/--bindings'
(/defun* bindings (prefix bindings)
  "Return the `/--bindings'.

Binding are `(key def)' pairs. If PREFIX is non-nil, add the PREFIX to each
key in the bindings. If PREFIX is nil, do nothing.
We distinguish whether PREFIX is nil by counting the length of BINDINGS.
  - If the length is odd, PREFIX is non-nil. Construct a legal bindings by
    append PREFIX to the head of BINDINGS
  - If the length is even, PREFIX is non-nil. Then add PREFIX to each key
    in the BINDINGS.

For each binding pair (key def), the `/--keys' and `/--key-definition'
are used."
  (let* ((idx 0))
    (and (cl-oddp (length bindings))
	 (setq bindings (cons prefix bindings) prefix nil))
    (/seq-map-alternate bindings `(/--keys ,prefix) #'/--key-definition)))

(/defmacro* sexp-define-key (prefix bindings &rest body)
  "Evaluate BODY in `/--bindings'.
PREFIX and BINDINGS are `/--bindings' arguments.

The `/--bindings' are binded to variable `bindings'.
The expressions in BODY can refer to it by the variable."
  (declare (indent defun))
  `(if ,prefix
       (if ,bindings (let* ((bindings (/--bindings ,prefix ,bindings))) ,@body)
	 nil)
     (assert (null bindings)) nil))

(/defmacro* define-keys (keymap &rest bindings)
  "Extend `define-key' to multiple pairs of bindings.
KEYMAP is `/--keymap' argument.
BINDINGS is `/--bindings' argument."
  (declare (indent defun))
  (/--sexp-progn
    (while bindings
      (/--sexp-exec
	`(define-key ',(/--keymap keymap) ,(pop bindings) ,(pop bindings))))))

;; Define `/--key-bindings'
(/defmacro define-keys (keymap &optional prefix &rest bindings)
  "Define `/--key-bindings'.
The KEYAMP is `/--keymap' argument.
The PREFIX and BINDINGS are `/--bindings' arguments."
  (declare (indent defun))
  (message "define in: 1")
  (/--sexp-define-key prefix bindings `(/--define-keys ,keymap ,@bindings)))

;(message "keymap: aaa" )
;(/define-keys global-map "\C-c\C-c" #'/show-key-binding)
;(message "keymap: bbb" )

(/defmacro remove-keys (keymap &rest keys)
  "Remove `/--keys' KEYS from `/--keymap' KEYMAP.
KEYMAP is `/--keymap' argument.
KEYS are `/--keys' argument."
  (declare (indent defun))
  (/--sexp-progn
    (while keys (/--sexp-exec `(/remove-key ,keymap ,(/--keys (pop keys)))))))

(/defmacro* define-specific-keymap-macro (name keymap &optional doc)
  "Define macro to define keys in specific KEYMAP.
NAME is `/--name' argument.
KEYMAP is a symbol of keymap."
  (declare (indent defun))
  ;(message "define: %s %s" name keymap)
  `(/defmacro
     (define-keys ,name)
     ;((define-keys - %s) ,@(/--list name))
     (&optional prefix &rest bindings)
     ,(format "Define `/--key-bindings' in `%s'.
PREFIX and BINDINGS are `/--bindings' arguments.%s"
	      keymap (if doc (format "\n%s" doc) ""))
     `(/define-keys ,',keymap ,prefix ,@bindings)))

;; ;;; {{ Set specific keymaps
(/defmacro define-keys-mode (mode &optional prefix &rest bindings)
  "Define key bindings in `MODE-map'.
MODE is a `/--name' argument."
  (declare (indent defun))
  `(/define-keys ((%s - map) ,@(/--list mode)) ,prefix ,@bindings))

(/--define-specific-keymap-macro global global-map) 
;; (/--define-specific-keymap-macro ctl-x ctl-x-map)
;; (/--define-specific-keymap-macro ctl-c mode-specific-map)
;; (/--define-specific-keymap-macro ctl-h help-map)
;; (/--define-specific-keymap-macro meta-g goto-map)

;; (defmacro /def-keys-ctl-c-prefix (keymap &rest bindings)
;;   "Define keys in KEYMAP with prefix `C-c'."
;;   (declare (indent defun))
;;   `(/def-keys ,keymap C-c ,@bindings))

;; (defmacro /def-keys-ctl-c-mode (mode &rest bindings)
;;   "Define keys in `MODE-map' with prefix `C-c'."
;;   (declare (indent defun))
;;   `(/def-keys ,(/--intern-mode-map mode) C-c ,@bindings))
;; ;;; }}

;; (defun /--intern-transient-minor-mode (mode)
;;   "Intern `/MODE-transient-mode'.\nSee `/--intern'."
;;   (/--intern (/--intern-mode (format "%s-transient" (/--name mode)))))

;; (defun /--intern-transient-minor-mode-toggle (mode)
;;   (:documentation (format "Intern `%s-toggle'.\nSee `/--intern'."
;; 			  (/--intern-transient-minor-mode 'MODE)))
;;   (/--intern-format "%s-toggle" (/--intern-transient-minor-mode mode)))

;; (defmacro /def-transient-minor-mode (mode doc keymap &optional tag)
;;   "Define a transient minor mode using KEYMAP."
;;   (declare (doc-string 2) (indent defun))
;;   (let* ((mode (/--intern-transient-minor-mode mode))
;; 	 (toggle (/--intern-transient-minor-mode-toggle mode))
;; 	 (map (/--intern-mode-map mode)))
;;     (/--sexp-progn-exec
;;       `(define-minor-mode ,mode
;; 	 ,doc
;; 	 :init-value nil
;; 	 :lighter nil
;; 	 :global nil
;; 	 :keymap ,keymap
;; 	 (if ,mode
;; 	     (progn
;; 	       (set-transient-map ,map t ',toggle)
;; 	       (setq /custom-transient-minor-mode-mode-line-tag ,tag))
;; 	   (setq /custom-transient-minor-mode-mode-line-tag nil)))
;;       `(defun ,toggle ()
;; 	 ,(format "Toggle %s." mode)
;; 	 (call-interactively ',mode))
;;       `(/advice-add-silence ,mode))))


(/provide)
;;; meta/keymap.el ends here
