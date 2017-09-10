;;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

(eval-when-compile
  (require 'code))

(require 'hideshow-lib)
;; (require 'hideshow-org)

(code-add-hook
 (c-mode-common-hook
  emacs-lisp-mode-hook
  java-mode-hook
  lisp-mode-hook)
 hs-minor-mode
 ;; hs-org/minor-mode
 )

(code-eval-after-load
 hideshow
 ;; Hide initial comment block
 (code-add-hook
  (hs-minor-mode-hook)
  hs-hide-initial-comment-block)
 ;; Auto open hide block when goto line
 (code-add-advice
  (goto-line
   evil-goto-line) :after
   (lambda (&rest args)
     (save-excursion
       (hs-show-block)))))


(provide 'config-hideshow)
;;; config-hideshow.el ends here
