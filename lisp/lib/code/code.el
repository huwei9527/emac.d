;; -*- lexical-binding : t ;byte-compile-dynamic : t -*-

(require 'test-code)
(require 'util-lib)

(defmacro intern-format (ft &rest args)
  "Extend 'intern' with format FT string."
  `(intern (format ,ft ,@args)))

;;; {{ Helper macros to construct SEXP forms for 'defmacro'
;;;    Note: SEXP are evaluated once. e.g '(code-item SEXP)'
;;;          will evaluate SEXP and add the result to the
;;;          structure form like 'progn', 'cond' ... If you
;;;          want SEXP to be added literally, you have to
;;;          quote it like '(code-item 'SEXP)'
(defvar code--obarray (make-vector 11 0))
(defsubst code--gensym (name)
  "Create a symbol with name NAME interned in 'code--obarray'."
  (intern name code--obarray))

(defsubst code--gensym-sexp-list ()
  "Create a temporary symbol interned in 'code--obarray'."
  (code--gensym "code--sexp-list"))

(defmacro code-sexp (&rest body)
  "Create a form sexp."
  (declare (indent defun))
  (let* ((tmp (code--gensym-sexp-list)))
    `(let* ((,tmp nil))
       ,@body
       (nreverse ,tmp))))

(defmacro code--append-one (el &optional eval-p)
  "Append EL to form list."
  `(push ,el ,(code--gensym-sexp-list)))

(defmacro code-append (&rest els)
  "Append body to form list."
  (code-progn
   (dolist (el els)
     (code--append-one
      `(code--append-one ,el)))))

(defmacro code-progn (&rest body)
  "Create a 'progn' form."
  `(code-sexp
    (code--append-one 'progn)
    ,@body))

(defmacro code-item (&rest sexps)
  "Create sexp form in 'progn' form."
  `(code-append ,@sexps))

(defmacro code-cond (&rest body)
  "Create a 'cond' form."
  `(code-sexp
    (code--append-one 'cond)
    ,@body))

(defmacro code-case (cond-sexp &rest body)
  "Create a 'case' form in 'cond' form."
  `(code-append
    (code-sexp (code-append ,cond-sexp ,@body))))

(defmacro code-setq (&rest body)
  "Create a 'setq' form."
  `(code-sexp
    (code--append-one 'setq)
    ,@body))

(defmacro code-pair (key value &rest pairs)
  "Create a 'key value' item in 'setq' form."
  (code-progn
   (code-item `(code-append ,key ,value))
   (while pairs (code-item `(code-pair ,(pop pairs) ,(pop pairs))))))

(defun code-parse-list (sexp op)
  "Parse the list, take operator on each element recursively."
  (let* ((stack (list (cons sexp nil)))
	 stack-curr sexp-curr sexp-parent sexp-parser
	 path op-rlt)
    (while stack
      (setq stack-curr (pop stack)
	    sexp-curr (car stack-curr)
	    sexp-parent (cdr stack-curr))
      (while (not (eq sexp-parent (car path)))
	(pop path))
      (when (and (funcall op sexp-curr path)
		 (listp sexp-curr))
	(setq sexp-parser sexp-curr)
	(while sexp-parser
	  (push (cons (car sexp-parser) sexp-curr) stack)
	  (setq sexp-parser (cdr sexp-parser))))
      (push sexp-curr path))))

(defvar code-macro-list nil
  "The list of macros defined in 'code'.")
(defmacro code-record-macro (macro)
  "Record MACRO in 'code-macro-list'"
  `(push ',macro code-macro-list))
(defmacro code-expandmacro-p (macro)
  "Non-nil if MACRO is in 'code-macro-list'"
  (if (symbolp macro)
      `(memq ',macro code-macro-list)
    `(memq ,macro code-macro-list)))
(defun code-expandmacro (sexp pos path)
  "Expand SEXP if it is a 'code' macro."
  (when (listp sexp)
    (let* ((sexp-head (car sexp)))
      (when (and (not (listp sexp-head))
		 (string-prefix-p "code-" (symbol-name sexp-head))
		 (not (code-expandmacro-p (car sexp))))
	(display-warning 'code-not-tracked
			 (format "code macro not tracked: %s" sexp-head)))))
  (if (and (listp sexp) (code-expandmacro-p (car sexp)))
      (progn (setf (elt (car path) pos) (macroexpand-all sexp)) nil)
    t))

(defmacro code-eval-after-load (package &rest body)
  "Run body after PACHAGE loads.

This extend 'eval-after-load' to 'code' macros which is useful
in byte compilation."
  (code-sexp
    (code-append 'eval-after-load)
    (if (symbolp package)
	(code-append `(quote ,package))
      (code-append package))
    (sequence-element-filter body 'code-expandmacro)
    (code-append
     `(quote
       ,(code-progn
	 (dolist (form body)
	   (code-item form)))))))

;; }}


;;; {{ Helper function to add advices or hooks for one function.
(defsubst code--gensym-symbol-list ()
  "Create temporary symbol interned in 'code-obarray'"
  (code--gensym "code--temporary-symbol"))

(defmacro code-get-list (sym)
  "Get the list content of SYM."
  (let* ((tempvar (code--gensym-symbol-list)))
    `(let* ((,tempvar ,sym))
       (while (symbolp ,tempvar)
         (setq ,tempvar (symbol-value ,tempvar)))
       ,tempvar)))

(defmacro code-add-advice (ad-list ad-where ad-fun &rest funs)
  "Add advice function AD-FUN to functions in AD-LIST."
  (code-progn
   (let* ((sym-list (code-get-list ad-list))
          (sym-fun ad-fun)
          (sym-where ad-where))
     (dolist (ad sym-list)
       (code-item `(advice-add ',ad ,sym-where #',sym-fun)))
     (while funs
       (code-item `(code-add-advice ,ad-list ,ad-where ,(pop funs)))))))
(code-record-macro code-add-advice)

(defmacro code-remove-advice (ad-list ad-fun &rest funs)
  "Remove advice function AD-FUN from functions in AD-LIST"
  (code-progn
   (let* ((sym-list (code-get-list ad-list))
          (sym-fun ad-fun))
     (dolist (ad sym-list)
       (code-item `(advice-remove ',ad #',sym-fun)))
     (while funs
       (code-item `(code-remove-advice ,ad-list ,(pop funs)))))))

(defmacro code-add-hook (hk-list hk-fun &rest funs)
  "Add hook function HK-FUN to hooks in HK-LIST"
  (code-progn
   (let* ((sym-list (code-get-list hk-list))
          (sym-fun hk-fun))
     (dolist (hk sym-list)
       (code-item `(add-hook ',hk #',sym-fun)))
     (while funs
       (code-item `(code-add-hook ,hk-list ,(pop funs)))))))
(code-record-macro code-add-hook)

(defmacro code-remove-hook (hk-list hk-fun &rest funs)
  "Remove hook function HK-FUN to hooks in HK-LIST"
  (code-progn
   (let* ((sym-list (code-get-list hk-list))
          (sym-fun hk-fun))
     (dolist (hk sym-list)
       (code-item `(remove-hook ',hk #',sym-fun)))
     (while funs
       (code-item `(code-remove-hook ,hk-list ,(pop funs)))))))

(defmacro code-add-advice-ignore (ad-list)
  "Add advice to ignore functions in AD-LIST."
  `(code-add-advice ,ad-list :override ignore))

(defmacro code-remove-advice-ignore (ad-list)
  "Remove advice to ignore functions in AD-LIST."
  `(code-remove-advice ,ad-list ignore))

(defmacro code-defsetter-command-advice (cmd-name cmd-list)
  "Define macro to set up or remove advice for comand in CMD-LIST."
  (code-progn
   (code-item
    `(defmacro ,(intern-format "code-add-advice-for-%s-command" cmd-name)
         (ad-where ad-fun &rest funs)
       ,(format "Add advice AD-FUN to %s commands." cmd-name)
       `(code-add-advice ,,cmd-list ,ad-where ,ad-fun ,@funs))
    `(defmacro ,(intern-format "code-remove-advice-for-%s-command" cmd-name)
         (ad-fun &rest funs)
       ,(format "Remove advice AD-FUN from %s commands." cmd-name)
       `(code-remove-advice ,,cmd-list ,ad-fun ,@funs)))))

(defmacro code-defsetter-hook (hk-name hk-list)
  "Define macro to set up or remove hook for hooks in HK-LIST."
  (code-progn
   (code-item
    `(defmacro ,(intern-format "code-add-hook-for-%s" hk-name)
         (hk-fun &rest funs)
       ,(format "Add hook HK-FUN to %s hooks." hk-name)
       `(code-add-hook ,,hk-list ,hk-fun ,@funs))
    `(defmacro ,(intern-format "code-remove-hook-for-%s" hk-name)
         (hk-fun &rest funs)
       ,(format "Remove hook HK-FUN from %s hooks." hk-name)
       `(code-remove-hook ,,hk-list ,hk-fun ,@funs)))))

(code-defsetter-command-advice "buffer-change" '(switch-to-buffer))
(code-defsetter-command-advice "window-switch"
                               '(other-window windmove-left windmove-right
                                              windmove-up windmove-down))
(code-defsetter-hook "emacs-out" '(focus-out-hook suspend-hook))
;; }}

;;; {{ Create key bindings
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
;; }}

(provide 'code)
;; code.el ends here
