;; -*- lexical-binding : t ;byte-compile-dynamic : t -*-

(eval-when-compile
  (require 'code))

(let* ((cnt 0))
  (defun code-test-key-binding ()
    "Test key bingding"
    (interactive)
    (message "%s[%d]: (k: %s) (v: %s)"
	     this-command (setq cnt (1+ cnt))
	     (this-command-keys) (this-command-keys-vector) )))

(defun code-make-key (key &optional prefix)
  "Combine PREFIX and KEY to form a new key sequence. It also 
uniformed the format of the key sequence."
  (if prefix
      (progn
        (setq prefix (code-make-key prefix))
        (setq key (code-make-key key))
        (vconcat prefix key))
    (if (vectorp key) key (kbd key))))

(defmacro code-define-key-raw (keymap prefix key def)
  "Define key binding in keymap."
  (declare (indent defun))
  (let* ((ks (code-make-key key prefix)))
    (cond
     ((null def)
      `(define-key ,keymap ,ks #'undefined))
     ((symbolp def)
      `(define-key ,keymap ,ks #',def))
     ((stringp def)
      `(define-key ,keymap ,ks ,(kbd def)))
     ((vectorp def)
      `(define-key ,keymap ,ks ,def))
     ((and (listp def))
      `(define-key ,keymap ,ks ,(eval def)))
     (t
      (error "Undefined binding: %s" def)
      nil))))

(defmacro code-define-key (keymap prefix key def &rest bindings)
  "Define key bindings in keymap."
  (declare (indent defun))
  (code-progn
   (code-item
    `(code-define-key-raw ,keymap ,prefix ,key ,def))
   (while bindings
     (code-item
      `(code-define-key-raw ,keymap ,prefix ,(pop bindings) ,(pop bindings))))))
(code-record-macro code-define-key)

(defmacro code-defkey-global (prefix key def &rest bindings)
  "Set global map. (current-global-map)"
  (declare (indent defun))
  (code-progn
   (code-item
    `(code-define-key-raw (current-global-map) ,prefix ,key ,def))
   (while bindings
     (code-item
      `(code-define-key-raw (current-global-map)
                            ,prefix ,(pop bindings) ,(pop bindings))))))

(defmacro code-remove-key (keymap key &rest keys)
  "Delete KEY and KEYS from keymap."
  (declare (indent defun))
  (code-progn
   (code-item
    `(assq-delete-all ,key ,keymap))
   (while keys
     `(assq-delete-all ,(pop keys) ,keymap))))

(defmacro code-defkey-ctl-x (key def &rest bindings)
  "Set ctl-x-map. 'C-x ...'"
  (declare (indent defun))
  `(code-define-key ctl-x-map nil ,key ,def ,@bindings))

(defmacro code-defkey-ctl-h (key def &rest bindings)
  "Set help-map. 'C-h ...'"
  `(code-define-key help-map nil ,key ,def ,@bindings))

(defmacro code-defkey-meta-g (key def &rest bindings)
  "Set goto-map. 'M-g ...'"
  `(code-define-key goto-map nil ,key ,def ,@bindings))

(defmacro code-defkey-ctl-c (key def &rest bindings)
  "Set mode-specific-map. 'C-c ...'"
  `(code-define-key mode-specific-map nil ,key ,def ,@bindings))

(defmacro code-defkey-ctl-c-local (keymap key def &rest bindings)
  "Set keymap for major mode with leader key 'C-c'"
  `(code-define-key ,keymap "C-c" ,key ,def ,@bindings))
(code-record-macro code-defkey-ctl-c-local)

(provide 'keymap-code)
;;; keymap-code.el ends here
