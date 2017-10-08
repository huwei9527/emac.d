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
(code-record-macro code-silence)

(defmacro code-silence-function (funs &rest body)
  "Run BODY when setting FUNS to 'ignore'"
  `(progn
     (code-add-advice-ignore ,funs)
     (unwind-protect
	 (progn
	   ,@body)
       (code-remove-advice-ignore ,funs))))
(code-record-macro code-silence-function)

(defmacro code-silence-function-true (funs &rest body)
  "Run BODY when setting FUNS to 'ignore-true'"
  `(progn
     (code-add-advice-ignore-true ,funs)
     (unwind-protect
	 (progn
	   ,@body)
       (code-remove-advice-ignore-true ,funs))))

(defmacro code-silence-y-or-n-p-with-y (&rest body)
  "Run BODY with 'y-or-n-p' returning t."
  `(code-silence-function-true (y-or-n-p) ,@body))

(defmacro code-silence-y-or-n-p-with-n (&rest bocy)
  "Run BODY with 'y-or-n-p' returning nil."
  `(code-silence-function (y-or-n-p) ,@body))

(provide 'silence-code)
;;; silence-code.el ends here
