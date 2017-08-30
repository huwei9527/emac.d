;; -*- lexical-binding : t byte-compile-dynamic : t -*-


(defun message-null (&rest args) "Dummy function." nil)

(defun message-mute ()
  "Mute message function."
  (advice-add 'message :override #'message-null)
  (advice-add 'minibuffer-message :override #'message-null)
  (advice-add 'message-box :override #'message-null)
  (advice-add 'message-or-box :override #'message-null)
  (advice-add 'display-message-or-buffer :override #'message-null)
  (advice-add 'print :override #'message-null)
  (advice-add 'prin1 :override #'message-null)
  (advice-add 'princ :override #'message-null)
  (advice-add 'pp :override #'message-null))

(defun message-unmute ()
  "Unmute message function."
  (advice-remove 'message #'message-null)
  (advice-remove 'minibuffer-message #'message-null)
  (advice-remove 'mesage-box #'message-null)
  (advice-remove 'message-or-box #'message-null)
  (advice-remove 'display-message-or-buffer #'message-null)
  (advice-remove 'print #'message-null)
  (advice-remove 'prin1 #'message-null)
  (advice-remove 'princ #'message-null)
  (advice-remove 'pp #'message-null))

(defmacro with-no-message (&rest form)
  "Run FORM with no message."
  `(progn
     (let* ((message-log-max nil)
            (standard-output nil))
       (message-mute)
       (unwind-protect
           (with-no-warnings
             (with-output-to-temp-buffer "*NULL*"
               ,@form))
         (message-unmute)))))

(defsubst char-spacep (c)
  "Return t if C is a space character."
  (eq ?\s (char-syntax c)))

(provide 'util-lib)
; utility.el ends here