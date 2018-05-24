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
  (message "smile2"))


(defvar /adlist '(smile ignore))
(defvar /flist '(switch-to-buffer other-window))
(defvar /hlist '(focus-out-hook suspend-hook))

(/advice-add /flist :before smile1 smile2)
(/advice-remove /flist :before smile1 smile2)
(/add-hook /hlist smile1 smile2)
(/remove-hook /hlist smile2 smile2)
(/advice-add-false switch-to-buffer)
(/advice-remove-false switch-to-buffer)
(/advice-add-true switch-to-buffer)
(/advice-remove-true switch-to-buffer)
(/--def-advice-setter buffer-change switch-to-buffer other-window)
(/advice-add-buffer-change :after smile1 smile2)
(/advice-remove-buffer-change :after smile1 smile2)
(/--def-hook-setter focus-out focus-out-hook suspend-hook)
(/add-hook-focus-out smile1 smile2)
(/remove-hook-focus-out smile1 smile2)
;; (advice-add 'switch-to-buffer :after #'/false)
;; (advice-add 'other-window :before #'ignore)
;; (advice-add 'smile :before #'ignore)
;; (advice-add 'previous-buffer :after #'ignore)
(/progn-silently (message "a") (message "b"))

(/message-test-end)

(/provide)
;;; test/hook.el ends here
