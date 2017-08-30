;; -*- lexical-binding : t ;byte-compile-dynamic : t -*-

(require 'test-code)

(defun quotep (sexp)
  ""
  (message "%s" (car sexp)))


(let* ((pa '(a b c d)))
  (quotep pa))

(defun backquotep (sexp)
  ""
  nil)

(defmacro intern-format (ft &rest args)
  "Extend 'intern' with format FT string.

\(intern (format FT ...)) "
  `(intern (format ,ft ,@args)))

;;; {{ Helper macros to construct SEXP forms for 'defmacro'
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

(defmacro code--append-one (el)
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
    `(,',cond-sexp ,@',body)))

;; (pp (code-cond (code-case `(minibufferp) `(message "abcd") `(message "abcd"))))
;; (pp-macroexpand-all (code-case (minibufferp) (message "abcd") (message "efgh")))

(defmacro code-setq (&rest body)
  "Create a 'setq' form."
  `(code-sexp
    (code--append-one 'setq)
    ,@body))

(defmacro code-pair (key value)
  "Create a 'key value' item in 'setq' form."
  `(code-append ,key ,value))

(pp (code-setq (code-pair 'a 1) (code-pair 'b 2)))


(defmacro code-cond (&rest args-forms)
  "Construct progn forms block list with ARGS-FORMS.

This is a wrapper call '(code-block cond)'"
  `(code-block cond ,@args-forms))

(defmacro code-case (cond-sexp &rest arg-forms)
  "Add cond case form (COND-SEXP ,@ARG-FROMS) to cond list.

It's a helper function to construct cond form."
  `(code-push (list ,cond-sexp ,@arg-forms)))
;; }}

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

;;; {{ Helper function to add advices or hooks for one function.
(defmacro code-add-advice (ad-list ad-where ad-fun)
  "Add advice function AD-FUN to functions in AD-LIST."
  (code-progn
   (let* ((sym-list (code-get-list ad-list))
          (sym-fun ad-fun)
          (sym-where ad-where))
     (dolist (ad sym-list)
       (code-item `(advice-add ',ad ,sym-where #',sym-fun))))))

(defmacro code-remove-advice (ad-list ad-fun)
  "Remove advice function AD-FUN from functions in AD-LIST"
  (code-progn
   (let* ((sym-list (code-get-list ad-list))
          (sym-fun ad-fun))
     (dolist (ad sym-list)
       (code-item `(advice-remove ',ad #',sym-fun))))))

(defmacro code-add-hook (hk-list hk-fun)
  "Add hook function HK-FUN to hooks in HK-LIST"
  (code-progn
   (let* ((sym-list (code-get-list hk-list))
          (sym-fun hk-fun))
     (dolist (hk sym-list)
       (code-item `(add-hook ',hk #',sym-fun))))))

(defmacro code-remove-hook (hk-list hk-fun)
  "Remove hook function HK-FUN to hooks in HK-LIST"
  (code-progn
   (let* ((sym-list (code-get-list hk-list))
          (sym-fun hk-fun))
     (dolist (hk sym-list)
       (code-item `(remove-hook ',hk #',sym-fun))))))

(defmacro code-defsetter-command-advice (cmd-name cmd-list)
  "Define macro to set up or remove advice for comand in CMD-LIST."
  (code-progn
   (code-item
    `(defmacro ,(intern-format "code-add-advice-for-%s-command" cmd-name)
         (ad-fun ad-where)
       ,(format "Add advice AD-FUN to %s commands." cmd-name)
       `(code-add-advice ,,cmd-list ,ad-where ,ad-fun))
    `(defmacro ,(intern-format "code-remove-advice-for-%s-command" cmd-name)
         (ad-fun)
       ,(format "Remove advice AD-FUN from %s commands." cmd-name)
       `(code-remove-advice ,,cmd-list ,ad-fun)))))

(defmacro code-defsetter-hook (hk-name hk-list)
  "Define macro to set up or remove hook for hooks in HK-LIST."
  (code-progn
   (code-item
    `(defmacro ,(intern-format "code-add-hook-for-%s" hk-name)
         (hk-fun)
       ,(format "Add hook HK-FUN to %s hooks." hk-name)
       `(code-add-hook ,,hk-list ,hk-fun))
    `(defmacro ,(intern-format "code-remove-hook-for-%s" hk-name)
         (hk-fun)
       ,(format "Remove hook HK-FUN from %s hooks." hk-name)
       `(code-remove-hook ,,hk-list ,hk-fun)))))

(code-defsetter-command-advice "buffer-change" '(switch-to-buffer))
(code-defsetter-command-advice "window-switch"
                               '(other-window windmove-left windmove-right
                                              windmove-up windmove-down))
(code-defsetter-hook "emacs-out" '(focus-out-hook suspend-hook))
;; }}

(provide 'code)
;; code.el ends here