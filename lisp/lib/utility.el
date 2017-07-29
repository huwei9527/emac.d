;; -*- lexical-binding : t -*-

(defun message-null (&rest args) "Function doing nothing." nil)

(defun message-mute ()
  "Mute message function."
  (advice-add 'message :override #'message-null))

(defun message-unmute ()
  "Unmute message function."
  (advice-remove 'message #'message-null))

(defmacro with-no-message (&rest form)
  "Run FORM with no message."
  `(progn
     (let ((message-log-max nil) (standard-output nil))
       (message-mute)
       (unwind-protect
           (with-no-warnings (with-output-to-temp-buffer "*NULL*" ,@form))
         (message-unmute)))))

(provide 'utility)
; utility.el ends here