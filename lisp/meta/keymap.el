;;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

;;; Commentary:

;;; Code:

(/require-lib core)

(defun /--kbd (keys)
  "Extend `kbd' to symbols."
  (declare (indent defun))
  (kbd (/--name keys)))

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

(defmacro /--def-keys (keymap prefix keys &optional def)
  "Binding (PREFIX KEYS) to DEF in KEYMAP."
  (declare (indent defun))
  (let* ((keys (/--key-sequence keys prefix))
	 (def (cond
	       ((symbolp def) `(function ,def))
	       ((stringp def) (/--key-sequence def))
	       ;((null def) def) ((vectorp def) def) ((listp def) def)
	       ;((t (error "Unknown binding: %s" def)))
	       (t def))))
    `(define-key ,keymap ,keys ,def)))

(defmacro /def-keys (keymap &optional prefix &rest bindings)
  "Do the binding BINDINGS in PRIFIX map in KEYMAP.
If PREFIX is omitted, then the length of BINDINGS must be odd since
  PREFIX is counted as the start of bindings."
  (declare (indent defun))
  (if prefix
      (if bindings
	  (/--sexp-progn
	    ;; Take care the case when PREFIX is ommited.
	    ;; It is determined by the length of BINDINGS.
	    (when (/oddp (length bindings))
	      (/--sexp-exec
		`(/--def-keys ,keymap nil ,prefix ,(pop bindings)))
	      (setq prefix nil))
	    (while bindings
	      (/--sexp-exec
		`(/--def-keys ,keymap ,prefix
		   ,(pop bindings) ,(pop bindings)))))
	nil)
    (assert (null bindings))))

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
