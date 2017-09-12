;; -*- lexical-binding : t byte-compile-dynamic : t -*-

(eval-when-compile
  (require 'code))

(defmacro code-defcmd-double-events (cmd doc act-sig act-dou &optional delay)
  "Define command with double events feature.

If two duplicate events come in a short time, the command behaves
diffently."
  (declare (indent defun))
  (or delay (setq delay 'double-events-delay))
  `(defun ,cmd ()
     ,doc
     (interactive)
     (let* ((key (this-command-keys-vector))
	    (len-key (length key))
	    (events (wait-for-event len-key ,delay)))
       (if (equal key events)
	   (progn ,@act-dou)
	 ,@act-sig
	 (put-back-event events 'force)))))


(provide 'miscellany-code)
;;; miscellany-code.el ends here
