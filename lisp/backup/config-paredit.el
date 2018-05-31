;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

(eval-when-compile
  (require 'hook-code))

(code-add-hook
 (emacs-lisp-mode-hook
  eval-expression-minibuffer-setup-hook
  lisp-interaction-mode-hook)
 paredit-mode)

(provide 'config-paredit)
;;; config-paredit.el ends here
