;; -*- lexical-binding : t byte-compile-dynamic : t -*-

(defsubst code-evil-mode-line-color-name (st pos)
  "The variable symbol name for mode line color of evil state ST.

\(evil-[ST]-state-mode-line-[POS])"
  (format "evil-%s-state-mode-line-%s" st pos))

(defsubst code-evil-mode-line-setter-name (st)
  "The function name for setting mode line for evil state ST.

\(evil-[ST]-state-set-mode-line)"
  (format "evil-%s-state-set-mode-line" st))

(defmacro code-defcustom-evil-mode-line (st pos color)
  "Create custom variable for mode line color of evil state ST.

\(defcustom evil-[ST]-state-mode-line-[POS] COLOR)"
  (let* ((str-state st)
         (str-pos pos)
         (str-color color)
         (sb-name (intern (code-evil-mode-line-color-name str-state str-pos))))
  `(defcustom ,sb-name ,str-color
     ,(format "Modeline %s color for evil %s state." str-pos str-state)
     :type 'string
     :group 'config-group)))

(defmacro code-defsetter-evil-mode-line-state (st &optional fg bg)
  "Create variable and function for setting mode line for evil state ST.

\(defcustom evil-[ST]-state-mode-line-foreground FG)
\(defcustom evil-[ST]-state-mode-line-background BG)
\(defsubst set-mode-line-for-evil-[ST]-state ())"
  (let* ((str-state st)
         (str-fg fg)
         (str-bg bg)
         (str-cfg "foreground")
         (str-cbg "background")
         (sb-set (intern (code-evil-mode-line-setter-name str-state)))
         (sb-cfg (intern (code-evil-mode-line-color-name str-state str-cfg)))
         (sb-cbg (intern (code-evil-mode-line-color-name str-state str-cbg))))
    `(progn
       (code-defcustom-evil-mode-line ,str-state ,str-cfg ,str-fg)
       (code-defcustom-evil-mode-line ,str-state ,str-cbg ,str-bg)
       (defsubst ,sb-set ()
         ,(format "Modeline color setting function for evil %s state."
                  str-state)
         (set-mode-line-color ,sb-cfg ,sb-cbg)))))

(defmacro code-defsetter-evil-mode-line-state-all ()
  "Create mode line setting variables and functions for all evil state.

Just a wrapper for all state."
  (let* ((exp-list (list 'progn)))
    (dolist (cus evil-custom-mode-line-alist)
      (push `(code-defsetter-evil-mode-line-state
              ,(car cus) ,(cadr cus) ,(cddr cus)) exp-list))
    (nreverse exp-list)))

(defmacro code-defsetter-evil-mode-line ()
  "Create mode line setting function.

defsubst set-mode-line-for-evil ()
 (cond
  ((evil-insert-state-p) (set-mode-line-for-evil-insert-state))
   ...) "
  (let* ((exp-list
          (nreverse
           (list 'defun 'evil-custom-set-mode-line nil
                 "Set mode line by evil state.")))
         (cond-list (list 'cond)))
    (dolist (cus evil-custom-mode-line-alist)
      (push `((,(intern (format "evil-%s-state-p" (car cus))))
              (,(intern (code-evil-mode-line-setter-name (car cus)))))
            cond-list))
    (push `(t (set-mode-line-color)) cond-list)
    (nreverse (cons (nreverse cond-list) exp-list))))

(provide 'evil-code)
; evil-code.el ends here