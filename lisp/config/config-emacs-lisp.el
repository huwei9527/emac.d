;;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

(eval-when-compile
  (require 'code))

(eval-after-load 'lisp-mode
  `(progn
     ,(macroexpand-all
       '(code-defkey-ctl-c-local
	 emacs-lisp-mode-map
	 "c" checkdoc))))

;; eldoc-mode
(code-add-hook (emacs-lisp-mode-hook) eldoc-mode)

;; hightlight current sexp
(code-add-hook
 (emacs-lisp-mode-hook
  lisp-interaction-mode-hook)
 hl-sexp-mode)

;;; company
(eval-after-load 'company
  `(progn
     ,(macroexpand-all
       '(code-add-hook
	 (emacs-lisp-mode-hook
	  lisp-interaction-mode-hook)
	 (lambda ()
	   (setq company-backends
		 '((company-capf company-dabbrev-code company-keywords)
		   )))))))


(provide 'config-emacs-lisp)

;;; config-emacs-lisp.el ends here
