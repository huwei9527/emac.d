;;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

;;; Commentary:

;;; Code:

(eval-when-compile (/require-meta hook keymap))

(/require-lib file)

(/eval-after-load lisp-mode
  (/def-keys-ctl-c-mode emacs-lisp-mode c checkdoc)
  (/add-hook (emacs-lisp-mode-hook)
    eldoc-mode show-paren-mode
    (lambda ()
      (unless (/scratch-buffer-p)
	(company-mode 1)
	(set (make-local-variable 'company-backends)
	     '((company-capf company-abbrev-code company-keywords)))))))

(/provide)
;;; config/emacs-lisp.el ends here
