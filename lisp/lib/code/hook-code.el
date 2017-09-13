;; -*- lexical-binding : t ;byte-compile-dynamic : t -*-

(eval-when-compile
  (require 'code))

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

(provide 'hook-code)
;;; hook-code.el ends here
