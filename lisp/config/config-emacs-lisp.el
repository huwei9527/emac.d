;;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-


(eval-when-compile
  (require 'code))

(eval-after-load 'lisp-mode
  `(progn
     ,(macroexpand-all
       '(code-defkey-ctl-c-local
	 emacs-lisp-mode-map
	 "c" checkdoc))))

(provide 'config-emacs-lisp)

;;; config-emacs-lisp.el ends here
