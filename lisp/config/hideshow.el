;;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

;;; Commentary:

;;; Code:

(eval-when-compile (/require-meta core hook))

(/add-hook (c-mode-common-hook
	    emacs-lisp-mode-hook
	    java-mode-hook
	    lisp-mode-hook)
  hs-minor-mode)

(/eval-after-load hideshow
  (/add-hook (hs-minor-mode-hook) hs-hide-initial-comment-block)
  (/advice-add (goto-line evil-goto-line) :after
    (lambda (&rest args) (save-excursion (hs-show-block)))))

(/provide)
;;; config/hideshow.el ends here
