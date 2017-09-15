;; -*- lexical-binding : t byte-compile-dynamic : t -*-

(eval-when-compile
  (require 'hook-code))

;;;  Can't silence "Write file" message
(defmacro code--define-silence-trigger (out-funs)
  "Define silence trigger macro pairs."
  `(progn
     (defmacro code-silence-on ()
       "Silent stdout and stderr output"
       '(code-add-advice-ignore ,out-funs))
     (defmacro code-silence-off ()
       "Unmute stdout and stderr output"
       '(code-remove-advice-ignore ,out-funs))))

(code--define-silence-trigger
 (message
  minibuffer-message
  ;; message-box
  ;; message-or-box
  ;; display-message-or-buffer
  ;; print
  ;; prin1
  ;; princ
  ;; pp
  ))

(defmacro code-silence (&rest body)
  "Run BODY with no message."
  `(progn
     (let* ((standard-output nil) ;;(message-log-max nil)
	    )
       (code-silence-on)
       (unwind-protect
           (with-no-warnings
             ,@body)
         (code-silence-off)))))

(defmacro code-silence-function (funs &rest body)
  "Run BODY when setting FUNS to ignore"
  `(progn
     (code-add-advice-ignore ,funs)
     (unwind-protect
	 (progn
	   ,@body)
       (code-remove-advice-ignore ,funs))))

(provide 'silence-code)
;;; silence-code.el ends here
