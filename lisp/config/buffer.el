;;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

;;; Commentary:

;;; Code:

(eval-when-compile (/require-meta hook))

(/require-custom buffer)
(/require-lib buffer)

;; Ingore error
(setq command-error-function
      (lambda (data context caller)
	"Ignore error"
	(let* (ignorep)
	  (catch 'break-tag
	    (dolist (pred /custom-ignore-error-predicate-list)
	      (when (funcall pred data context caller)
		(setq ignorep t)
		(throw 'break-tag nil))))
	  (or ignorep (command-error-default-function
		       data context caller)))))

;; Don't show "Type C-x-1 to delete the help window" message
(/advice-add-true help-window-display-message)

;;; {{ Keymap
(/def-keys-global
 <backtab> mode-line-other-buffer  ; not in terminal
 <C-tab>   mode-line-other-buffer  ; not in terminal
 M-1       /close-other-window     ; 1 - one  ; 2 - all
 M-2       /split-window           ; 1 - open ; 2 - right
 M-3       /other-window           ; 3 - forward; 2 - backward
 C-q       /other-buffer           ; q - forward; tab, Q - backward
)
;;; }}

(/provide)
;;; config/buffer.el ends here
