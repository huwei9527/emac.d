;;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

;;; Commentary:

;;; Code:

(require 'macroexp)

(defun /--value (expr &optional depth)
  "Evaluate EXPR recursively at most DEPTH times.
If EXPR is a symbol, evaluate its symbol value if it is bounded,
otherwise return the symbol itself.  
If EXPR is not a symbol, evaluate it until it is an non-symbol
atom.\n
Note 1: The evaluation may fall into infinite loops if the symbol
values form a circle. So DEPTH is 65535 if not set explicitly and
properly (not positive integer type).
Note 2: If `lexical-binding' is in effect, whether a symbol is bounded is refers
to the global value outside of any lexical scope. So local variable such as
let-binding won't make a symbol as bounded."
  (or (and (integerp depth) (> depth 0)) (setq depth 65535))
  (while (not (eq depth 0))
    (if (symbolp expr)
	(if (boundp expr) (setq expr (eval expr)) (setq depth 1))
      (if (atom expr) (setq depth 1) (setq expr (eval expr))))
    (setq depth (1- depth)))
  expr)

(defun /--name (expr)
  "The string of EXPR.
If EXPR is a symbol, return the symbol name, otherwise return the printed
representation of `(/--value expr)'.\n
See `/--value'."
  (format "%s" (if (symbolp expr) expr (/--value expr))))

(defun /--require (module &rest feats)
  "Load features FEATS in module MODULE."
  (let* ((module (/--name module))
	 (dir (expand-file-name module
				(expand-file-name
				 "lisp" (file-truename user-emacs-directory)))))
    (dolist (feat feats)
      (let* ((name (/--name feat)))
	(require (intern (format "/%s/%s" module name))
		 (expand-file-name name dir))))))

(defun /--provide ()
  "Provide feature according the `load-file-name'.\n
If the `load-file-name' is .../XXX/YYY.el[c], then the feature provided is
  /XXX/YYY."
  (let ((str (file-name-sans-extension load-file-name)))
    (string-match "\\([^/]*\\)/\\([^/]*\\)\\'" str)
    (provide (intern (format "/%s/%s"
			     (match-string 1 str) (match-string 2 str))))))

;; Load basic macros
(eval-when-compile (/--require 'meta 'core))

(/provide)
;;; .el ends here
