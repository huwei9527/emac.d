;;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

;;; Commentary:

;;; Code:

(/require-meta hook)

(/message-test-start)

(defun smile1 (&rest args)
  ""
  ; (interactive "s")
  (message "smile1")
  )

(defun smile2 (&rest args)
  ""
  ;(interactive)
  (message "smile2"))


(defvar /adlist '(smile ignore))
(defvar /flist '(switch-to-buffer other-window))
(defvar /hlist '(focus-out-hook suspend-hook))

(defmacro macroaaa (list)
  ""
  (dolist (l list) (message "%s" l)))

(let* ((ll '(1 2 3 4)))
  ;(dolist (l (eval ll)) (message "%s" l))
  ;(macroaaa ll)
  )

(when t
  (/advice-add (switch-to-buffer other-window) :before smile1 smile2)
  (/advice-remove (switch-to-buffer other-window) :before smile1 smile2)
  (/add-hook (focus-out-hook suspend-hook) smile1 smile2)
  (/remove-hook (focus-out-hook suspend-hook) smile2 smile2)
  (/advice-add-false switch-to-buffer)
  (/advice-remove-false switch-to-buffer)
  (/advice-add-true switch-to-buffer)
  (/advice-remove-true switch-to-buffer)
  (/--define-advice-setter buffer-change switch-to-buffer other-window)
  (/advice-add-buffer-change :after smile1 smile2)
  (/advice-remove-buffer-change :after smile1 smile2)
  )

(when nil
  (/--define-hook-setter focus-out focus-out-hook suspend-hook)
  (/add-hook-focus-out smile1 smile2)
  (/remove-hook-focus-out smile1 smile2)
  ;; (advice-add 'switch-to-buffer :after #'/false)
  ;; (advice-add 'other-window :before #'ignore)
  ;; (advice-add 'smile :before #'ignore)
  ;; (advice-add 'previous-buffer :after #'ignore)
  (/progn-silently (message "a") (message "b"))
  (/with-no-message (message "a") (message "b"))
  (/with-yes (print (y-or-n-p "ABCD: ")) (print (yes-or-no-p "ABCD*: ")))
  (/with-no (print (y-or-n-p "ABCD: ")) (print (yes-or-no-p "ABCD*: ")))
  ; (print (y-or-n-p "DEFG:")) (print (yes-or-no-p "DEFG*:"))
)
(/message-test-end)

(/provide)
;;; test/hook.el ends here
