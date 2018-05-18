;;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

(eval-when-compile
  (require 'hook-code)
  (require 'keymap-code))

(require 'util-lib)

(code-eval-after-load
 lisp-mode
 (code-defkey-ctl-c-local
  emacs-lisp-mode-map
  "c" checkdoc)
 ;; eldoc-mode
 (code-add-hook (emacs-lisp-mode-hook) eldoc-mode)
 ;; hightlight current sexp
 (code-add-hook
  (emacs-lisp-mode-hook)
  ;; hl-sexp-mode
  ;; company-mode
  show-paren-mode
  (lambda ()
    (unless (scratch-buffer-p)
      (company-mode 1)
      (set (make-local-variable 'company-backends)
	   '((company-capf company-abbrev-code company-keywords)))))
  ))

(provide 'config-emacs-lisp)

;;; config-emacs-lisp.el ends here
