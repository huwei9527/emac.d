;; -*- lexical-binding : t ;byte-compile-dynamic : t -*-

(defun code-test-advice (&rest args)
  "Code test advice"
  (message "Test code test advice"))

(defvar advice-list
  '(switch-to-buffer select-window))

(defvar hook-list
  '(suspend-hook focus-out-hook))

(defmacro code-add-advice (ad-list ad-where ad-fun)
  "Add advice function AD-FUN to functions in AD-LIST."
  (let* ((exp-list (list 'progn))
         (sb-fun ad-fun)
         (sb-where ad-where))
    (dolist (ad (symbol-value ad-list))
      (push `(advice-add ',ad ,sb-where #',sb-fun) exp-list))
    (nreverse exp-list)))

(defmacro code-remove-advice (ad-list ad-fun)
  "Remove advice function AD-FUN from functions in AD-LIST"
  (let* ((exp-list (list 'progn))
         (sb-fun ad-fun))
    (dolist (ad (symbol-value ad-list))
      (push `(advice-remove ',ad #',sb-fun) exp-list))
    (nreverse exp-list)))

(defmacro code-add-hook (hk-list hk-fun)
  "Add hook function HK-FUN to hooks in HK-LIST"
  (let* ((exp-list (list 'progn))
         (sb-fun hk-fun))
    (dolist (hk (symbol-value hk-list))
      (push `(add-hook ',hk #',sb-fun) exp-list))
    (nreverse exp-list)))

(defmacro code-remove-hook (hk-list hk-fun)
  "Remove hook function HK-FUN to hooks in HK-LIST"
  (let* ((exp-list (list 'progn))
         (sb-fun hk-fun))
    (dolist (hk (symbol-value hk-list))
      (push `(remove-hook ',hk #',sb-fun) exp-list))
    (nreverse exp-list)))

(defvar code-buffer-change-command-list
  '(switch-to-buffer other-window windmove-left
    windmove-right windmove-up windmove-down)
  "The commands that change current buffer by user.")

(defmacro code-add-advice-for-buffer-change-command (ad-fun ad-where)
  "Add advice AD-FUN to buffer change command."
  `(code-add-advice code-buffer-change-command-list ,ad-where ,ad-fun))

(defmacro code-remove-advice-for-buffer-change-command (ad-fun)
  "Remove advice AD-FUN from buffer change command."
  `(code-remove-advice code-buffer-change-command-list ,ad-fun))

(defvar code-emacs-out-hook-list
  '(focus-out-hook suspend-hook))

(defmacro code-add-hook-for-emacs-out (hk-fun)
  "Add hook HK-FUN to emacs out hooks."
  `(code-add-hook code-emacs-out-hook-list ,hk-fun))

(defmacro code-remove-hook-for-emacs-out (hk-fun)
  "Remove hook HK-FUN from emacs out hooks."
  `(code-remove-hook code-emacs-out-hook-list ,hk-fun))

;; (pp (macroexpand-all '(code-add-advice advice-list :after code-test-advice)))
;; (pp (macroexpand-all '(code-add-hook hook-list code-test-advice)))
;; (pp (macroexpand-all '(code-remove-advice advice-list code-test-advice)))
;; (pp (macroexpand-all '(code-remove-hook hook-list code-test-advice)))
;; (pp (macroexpand '(code-add-advice-for-buffer-change-command code-test-advice :after)))
;; (pp (macroexpand '(code-remove-advice-for-buffer-change-command code-test-advice)))
(pp (macroexpand '(code-add-hook-for-emacs-out-hook code-test-advice)))
(pp (macroexpand '(code-remove-hook-for-emacs-out-hook code-test-advice)))

;; (code-add-advice advice-list :after code-test-advice)
;; (code-remove-advice advice-list code-test-advice)
;; (code-add-hook hook-list code-test-advice)
;; (code-remove-hook hook-list code-test-advice)
;; (code-add-advice-for-buffer-change-command code-test-advice :after)
;; (code-remove-advice-for-buffer-change-command code-test-advice)

(provide 'code)
;; code.el ends here