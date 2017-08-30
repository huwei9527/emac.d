;; -*- lexical-binding : t ;byte-compile-dynamic : t -*-

(require 'test-code)

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
  "Helper function for creating macro. Create a form sexp."
  (declare (indent defun))
  (let* ((tmp (code--gensym-sexp-list)))
    `(let* ((,tmp nil))
       ,@body
       (nreverse ,tmp))))

(defmacro code--append-one (el)
  "Helper function for creating macro. Append EL to form list."
  `(push ,el ,(code--gensym-sexp-list)))

(defmacro code-append (&rest els)
  "Helper function for creating macro. Append body to form list."
  (code-progn
   (dolist (el els)
     (code--append-one
      `(code--append-one ,el)))))

(defmacro code-progn (&rest body)
  "Helper function for creating macro. Create a 'progn' form."
  `(code-sexp
    (code--append-one 'progn)
    ,@body))

(defmacro code-push (&rest sexps)
  "Helper function for creating macro. Create item in 'progn' form."
  `(code-append ,@sexps))

(defmacro code-block (block-type &rest args-forms)
  "Construct forms block list with ARGS-FROMS of type BLOCK-TYPE.

BLOCK-TYPE can be 'progn', 'cond'
`(BLOCK-TYPE ,@ARGS-FORMS)."
  (let* ((tempvar (code--gensym-sexp-list)))
    `(let* ((,tempvar (list ',block-type)))
       ,@args-forms
       (nreverse ,tempvar))))

;; (defmacro code-progn (&rest args-forms)
;;   "Construct progn forms block list with ARGS-FORMS.

;; This is a wrapper call '(code-block progn)'"
;;   `(code-block progn ,@args-forms))

(defmacro code-cond (&rest args-forms)
  "Construct progn forms block list with ARGS-FORMS.

This is a wrapper call '(code-block cond)'"
  `(code-block cond ,@args-forms))

;; (defmacro code-push (sexp)
;;   "Add SEXP to local variable 'code-sexp-list'.

;; It's a helper function to construct progn form or cond form."
;;   `(push ,sexp ,(code--gensym-sexp-list)))

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
       (code-append `(advice-add ',ad ,sym-where #',sym-fun))))))

(pp-macroexpand (code-add-advice (a) :after test1))

(defmacro code-remove-advice (ad-list ad-fun)
  "Remove advice function AD-FUN from functions in AD-LIST"
  (code-progn
   (let* ((sym-list (code-get-list ad-list))
          (sym-fun ad-fun))
     (dolist (ad sym-list)
       (code-push `(advice-remove ',ad #',sym-fun))))))

(defmacro code-add-hook (hk-list hk-fun)
  "Add hook function HK-FUN to hooks in HK-LIST"
  (code-progn
   (let* ((sym-list (code-get-list hk-list))
          (sym-fun hk-fun))
     (dolist (hk sym-list)
       (code-push `(add-hook ',hk #',sym-fun))))))

(defmacro code-remove-hook (hk-list hk-fun)
  "Remove hook function HK-FUN to hooks in HK-LIST"
  (code-progn
   (let* ((sym-list (code-get-list hk-list))
          (sym-fun hk-fun))
     (dolist (hk sym-list)
       (code-push `(remove-hook ',hk #',sym-fun))))))

(defmacro code-defsetter-command-advice (cmd-name cmd-list)
  "Define macro to set up or remove advice for comand in CMD-LIST."
  (code-progn
   (code-push
    `(defmacro ,(intern-format "code-add-advice-for-%s-command" cmd-name)
         (ad-fun ad-where)
       ,(format "Add advice AD-FUN to %s commands." cmd-name)
       `(code-add-advice ,,cmd-list ,ad-where ,ad-fun)))
   (code-push
    `(defmacro ,(intern-format "code-remove-advice-for-%s-command" cmd-name)
         (ad-fun)
       ,(format "Remove advice AD-FUN from %s commands." cmd-name)
       `(code-remove-advice ,,cmd-list ,ad-fun)))))

(defmacro code-defsetter-hook (hk-name hk-list)
  "Define macro to set up or remove hook for hooks in HK-LIST."
  (code-progn
   (code-push
    `(defmacro ,(intern-format "code-add-hook-for-%s" hk-name)
         (hk-fun)
       ,(format "Add hook HK-FUN to %s hooks." hk-name)
       `(code-add-hook ,,hk-list ,hk-fun)))
   (code-push
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