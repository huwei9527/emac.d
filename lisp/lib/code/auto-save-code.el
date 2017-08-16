;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

(defun code-smart-auto-save-trigger-name (name str-trigger)
  "The trigger function name for smart-auto-save trigger NAME.

STR-TRIGGER is on / off."
  (format "smart-auto-save-%s-%s" name str-trigger))

(defun code-smart-auto-save-on-name (name)
  "The enable function name for hook NAME.

\(smart-auto-save-[NAME]-on)"
  (code-smart-auto-save-trigger-name name "on"))

(defun code-smart-auto-save-off-name (name)
  "The disable function name for hook NAME.

\(smart-auto-save-[NAME]-off)"
  (code-smart-auto-save-trigger-name name "off"))

(defmacro code-defhook-smart-auto-save (name)
  "Define a set of functions for setting hook NAME for
smart-auto-save.

NAME if a type of feature of smart-auto-save.
\(defsubst smart-auto-save-[NAME]-on)
\(defsubst smart-autos-ave-[NAME]-off)"
  (let* ((str-name name)
         (sb-hook (intern (format "%s-hook" str-name)))
         (sb-save 'smart-auto-save-all-buffers)
         (sb-on (intern (code-smart-auto-save-on-name str-name)))
         (sb-off (intern (code-smart-auto-save-off-name str-name))))
    `(progn
       (defsubst ,sb-on ()
         ,(format "The enable function for %s hook of 'smart-auto-save'"
                  str-name)
         (interactive)
         (add-hook ',sb-hook #',sb-save))
       (defsubst ,sb-off ()
         ,(format "The disable function for %s hook of 'smart-auto-save'"
                  str-name)
         (interactive)
         (remove-hook ',sb-hook #',sb-save)))))

(defmacro code-defhook-smart-auto-save-all ()
  "Define functions for setting hook for smart-auto-save.

Just a wrapper of 'code-defhook-smart-auto-save' for all hook in
'smart-auto-save-hook-triggers'"
  (let* ((exp-list (list 'progn)))
    (dolist (tri smart-auto-save-hook-triggers)
      (push `(code-defhook-smart-auto-save ,tri) exp-list))
    (nreverse exp-list)))

(defmacro code-defadvice-smart-auto-save (name)
  "Define a set of function for setting advice NAME for smart-auto-save.

\(defsubst smart-auto-save-[NAME]-on)
\(defsubst smart-auto-save-[NAME]-off)"
  (let* ((str-name name)
         (sb-advice (intern name))
         (sb-save 'smart-auto-save-buffer-advice)
         (sb-on (intern (code-smart-auto-save-on-name str-name)))
         (sb-off (intern (code-smart-auto-save-off-name str-name))))
    `(progn
       (defsubst ,sb-on ()
         ,(format "The enable function for %s advice of smart-auto-save"
                  str-name)
         (interactive)
         (advice-add ',sb-advice :before #',sb-save))
       (defsubst ,sb-off ()
         ,(format "The disable function for %s advice of smart-auto-save"
                  str-name)
         (interactive)
         (advice-remove ',sb-advice #',sb-save)))))

(defmacro code-defadvice-smart-auto-save-all ()
  "Define functions for setting advice for smart-auto-save.

Just a wrapper of 'code-defadvice-smart-auto-save' for all advice in
'smart-auto-save-advice-triggers'"
  (let* ((exp-list (list 'progn)))
    (dolist (tri smart-auto-save-advice-triggers)
      (push `(code-defadvice-smart-auto-save ,tri) exp-list))
    (nreverse exp-list)))

;(setq smart-auto-save-advice-triggers `("A" "B"))

(defsubst code-smart-auto-save-switch (type on-off)
  "Code for enabling or disabling advice or hook for smart-auto-save.

TYPE can be 'advice' or 'hook'.
ON-OFF can be 'on' or 'off'"
  (let* ((exp-list nil)
         (sb-tri (intern (format "smart-auto-save-%s-triggers" type)))
         (sb-fnm (intern (format "code-smart-auto-save-%s-name" on-off))))
    (dolist (tri (symbol-value sb-tri) exp-list)
      (push `(,(intern (funcall sb-fnm tri))) exp-list))))

(defmacro code-smart-auto-save-switch-all (on-off)
  "Generate function for enabling or disabling smart-auto-save.

ON-OFF 'on' or 'off'"
  (let* ((str-on on-off)
         (exp-list
          (nreverse
           (list 'defun
                 (intern (format "smart-auto-save-%s" str-on)) nil
                 (format "%s smart-auto-save."
                         (if (string= str-on "on")
                             "Enable"
                           "Disable"))
                 '(interactive)
                 `(,(intern (format "smart-auto-save-idle-%s" str-on)))))))
    ;(setq exp-list (nreverse exp-list))
    (dolist (el (code-smart-auto-save-switch "advice" str-on))
      (push el exp-list))
    (dolist (el (code-smart-auto-save-switch "hook" str-on))
      (push el exp-list))
    (setq exp-list (nreverse exp-list))))

(defmacro code-smart-auto-save ()
  "The enabling and disabling function for smart-auto-save.

Just a wrapper for 'on' and 'off' case.
\(code-smart-auto-save-switch-all 'on')
\(code-smart-auto-save-switch-all 'off')"
  `(progn
     (code-smart-auto-save-switch-all "on")
     (code-smart-auto-save-switch-all "off")))


(eval-when-compile
  (require 'code))


(provide 'auto-save-code)
; auto-save-code.el ends here