;;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

;;; Commentary:

;;; Code:

(/require-lib core)

(defun /--kbd (keys)
  "Extend `kbd' to symbol and character."
  (declare (indent defun))
  (if (characterp keys)
      (vector keys)
    (kbd (/--name keys))))

(defun /--key-sequence (keys &optional prefix)
  "Create new key sequence [PREFIX KEYS].
PREFIX and KEYS can be string or vector. If it is string, it will pass
  to `kbd' so the format is the same as `C-h k' shows.
The returned key sequence is vector."
  (if prefix
      (progn
	(or (vectorp prefix) (setq prefix (/--kbd prefix)))
	(or (vectorp keys) (setq keys (/--kbd keys)))
	(vconcat prefix keys))
    (if (vectorp keys) keys (string-to-vector (/--kbd keys)))))

(defun /--keymap (keymap)
  "Return the actual KEYMAP.
If KEYMAP is represented as a symbol, return its `symbol-function'."
  (if (keymapp keymap)
      (if (symbolp keymap) (symbol-function keymap) keymap)
    nil))

(defun /--lookup-key (keymap keys &optional prefix)
  "Lookup (PREFIX KEYS) in KEYMAP."
  (setq keymap (lookup-key keymap (/--key-sequence keys prefix)))
  ;; `lookup-key' returns a number if the KEYS is too long. Make it nil.
  (if (numberp keymap) nil keymap))

(defun /--key-definition (def)
  "Return the binding which can be used in keymaps."
  (declare (indent defun))
  (cond ((symbolp def) `(function ,def))
	((stringp def) (/--key-sequence def))
	((keymapp def) `(quote ,def))
	((listp def) (let* ((first (car def)))
		       (if (or (stringp first)     ; (STRING . DEFN)
			       (keymapp first)     ; (MAP    . CHAR)
			       (eq 'closure first) ; (CLOSURE)
			       ;(eq 'lambda first) lambda is eval to itself.
			       )
			   `(quote ,def)
			 def)))
	(t def)))

(defun /--bindings (prefix  bindings)
  "Construct bindings by add PREFIX to the keys in BINDINGS.
If PREFIX is omitted which happens when length of BINDINGS is
  odd (in this case PREFIX is counted as a key of key-binding
  pair), PREFIX is `push'ed to bindings.  
Otherwise, PREFIX is add to the keys in BINDINGS using
  `/--key-sequence'.  
In either case, key in key-binding is uniformed by `/--key-sequence'. 
the binding in key-binding pair is uniformed by `/--key-definition'."
  (let* ((idx 0))
    (when (/oddp (length bindings))
      (push prefix bindings)
      (setq prefix nil))
    (mapcar (lambda (e) (if (/oddp (setq idx (1+ idx)))
			    (/--key-sequence e prefix)
			  (/--key-definition e)))
	    bindings)))

(defmacro /--sexp-def-key (&rest body)
  "Evaluate BODY in the (prefix binding) settings.
See `/--bindings' for details of (prefix binding).
Only when prefix and bindings are both non-nil, uniformed the bindings
  and evaluate BODY."
  (declare (indent defun))
  `(if prefix
       (if bindings
	   (let* ((bindings (/--bindings prefix bindings)))
	     ,@body)
	 nil)
     (assert (null bindings))
     nil))

(defmacro /--def-keys (keymap &rest bindings)
  "Extend `define-key' to multiple pairs of bindings.
The key-binding pair in BINDINGS are note filtered by `/--key-sequence'
  and `/--key-definition'."
  (declare (indent defun))
  (/--sexp-progn
    (while bindings
      (/--sexp-exec
	`(define-key ,keymap ,(pop bindings) ,(pop bindings))))))

(defmacro /def-keys (keymap &optional prefix &rest bindings)
  "Define key bindings in BINDINGS with PREFIX in KEYMAP.
See `/--bindings' for argument usage."
  (declare (indent defun))
  (/--sexp-def-key `(/--def-keys ,keymap ,@bindings)))

(defun /--remove-keys (keymap keys &optional prefix)
  "Remove (PREFIX KEYS) from KEYMAP by removing the corresponding
elements."
  (let* ((keys (/--key-sequence keys prefix))
	 (len (1- (seq-length keys))))
    (and (> len 0)
	 (setq keymap
	       (/--keymap (/--lookup-key keymap (seq-take keys len)))))
    (and keymap (assq-delete-all (aref keys len) keymap))))

(defmacro /remove-keys (keymap &rest keys)
  "Remove KEYS from KEYMAP by removing the keymap elements.
This is different from assigning `nil' or `undefined' to that entry."
  (declare (indent defun))
  (/--sexp-progn
    (while keys
      (/--sexp-exec
	`(/--remove-keys ,keymap ,(/--key-sequence (pop keys)))))))

(defun /--intern-def-specific-keymap (name)
  "Intern `/def-keys-NAME'.\nSee `/--intern'."
  (declare (indent defun))
  (/--intern "def-keys-%s" (/--name name)))

(defun /--intern-mode-map (mode)
  "Intern `MODE-map'.\nSee `/--intern-format'."
  (declare (indent defun))
  (/--intern-format "%s-map" (/--name mode)))

(defmacro /--def-specific-keymap-macro (name keymap &optional doc)
  "Define macro to define keys in specific KEYMAP."
  (declare (indent defun))
  `(defmacro ,(/--intern-def-specific-keymap name)
       (&optional prefix &rest bindings)
     ,(format "Define keys in `%s'.
See `/def-keys' for argument usage.\n%s" keymap (if doc doc ""))
     `(/def-keys ,',keymap ,prefix ,@bindings)))

;;; {{ Set specific keymaps
(/--def-specific-keymap-macro global global-map)
(/--def-specific-keymap-macro ctl-x ctl-x-map)
(/--def-specific-keymap-macro ctl-c mode-specific-map)
(/--def-specific-keymap-macro ctl-h help-map)
(/--def-specific-keymap-macro meta-g goto-map)

(defmacro /def-keys-ctl-c-prefix (keymap &rest bindings)
  "Define keys in KEYMAP with prefix `C-c'."
  (declare (indent defun))
  `(/def-keys ,keymap C-c ,@bindings))

(defmacro /def-keys-ctl-c-mode (mode &rest bindings)
  "Define keys in `MODE-map' with prefix `C-c'."
  (declare (indent defun))
  `(/def-keys ,(/--intern-mode-map mode) C-c ,@bindings))
;;; }}


(/provide)
;;; meta/keymap.el ends here
