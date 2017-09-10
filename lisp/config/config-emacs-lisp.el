;;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

(eval-when-compile
  (require 'code))

(code-eval-after-load
 lisp-mode
 (code-defkey-ctl-c-local
  emacs-lisp-mode-map
  "c" checkdoc))

;; eldoc-mode
(code-add-hook (emacs-lisp-mode-hook) eldoc-mode)

;; hightlight current sexp
(code-add-hook
 (emacs-lisp-mode-hook
  lisp-interaction-mode-hook)
 hl-sexp-mode
 company-mode)

;;; company
(code-eval-after-load
 company
 (code-add-hook
  (emacs-lisp-mode-hook
   lisp-interaction-mode-hook)
  (lambda ()
    (setq company-backends
	  '((company-capf company-dabbrev-code company-keywords))))))

(provide 'config-emacs-lisp)

;;; config-emacs-lisp.el ends here
