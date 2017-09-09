;;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

(eval-when-compile
  (require 'code))

(require 'hideshow-lib)
(require 'hideshow-org)

(code-add-hook
 (c-mode-common-hook
  emacs-lisp-mode-hook
  java-mode-hook
  lisp-mode-hook)
 hs-minor-mode)


(provide 'config-hideshow)
;;; config-hideshow.el ends here
