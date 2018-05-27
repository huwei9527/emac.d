;;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

;;; Commentary:

;;; Code:

(eval-when-compile (/require-meta hook))

(/add-hook (emacs-lisp-mode-hook
	    eval-expression-minibuffer-setup-hook
	    lisp-interaction-mode-hook)
  paredit-mode)

(/provide)
;;; config/paredit.el ends here
