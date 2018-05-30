;;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

;;; Commentary:

;;; Code:

(eval-when-compile (/require-meta hook keymap company))

(/require-lib file)

(/eval-after-load lisp-mode
  (/def-keys-ctl-c-mode emacs-lisp-mode c checkdoc)
  (/add-hook (emacs-lisp-mode-hook)
    (lambda ()
      (eldoc-mode 1)
      (show-paren-mode 1)
      (unless (/scratch-buffer-p)
	(/company-mode-on
	 (company-capf company-abbrev-code company-keywords))))))

(/provide)
;;; config/emacs-lisp.el ends here
