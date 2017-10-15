;; -*- lexical-binding : t byte-compile-dynamic : t -*-

(eval-when-compile
  (require 'code))

(defmacro code-defcmd-double-events (cmd doc act-sin act-dou &optional delay)
  "Define command with double events feature.

If two duplicate events come in a short time, the command behaves
differently."
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
	 ,@act-sin
	 (put-back-event events 'force)))))

(defmacro code-define-temporary-minor-mode (mode doc keymap)
  "Define a temporary minor mode which use 'overriding-local-map' to 
shadow all the other major and minor mode keybinding.

If this minor mode is on, only keybindings in KEYMAP take effects. All 
the other keybindings are binded to exit this minor mode which would
clear 'overriding-local-map' to recover the bindings of other minor
modes. So it called 'temporary' minor mode."
  (let* ((mode (intern-format "%s-mode" (symbol-name mode)))
	 (map (intern-format "%s-map" (symbol-name mode))))
    `(define-minor-mode ,mode
       ,doc
       :global t
       :keymap `(,@,keymap
		 ([?q] . (lambda () (interactive) (,',mode -1)))
		 ([t] . (lambda () (interactive)
			  (,',mode -1)
			  (put-back-event nil 'force))))
       (if ,mode
	   (set-overriding-local-map ,map)
	 (recover-overriding-local-map ,map)))))

(provide 'miscellany-code)
;;; miscellany-code.el ends here
