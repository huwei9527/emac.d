;;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

;;; Commentary:

;;; Code:

(eval-when-compile (/require-meta hook keymap company))

(/require-lib file highlight-sexp)

(/eval-after-load lisp-mode
  (/def-keys-ctl-c-mode emacs-lisp-mode c checkdoc)
  (/add-hook (emacs-lisp-mode-hook)
    (lambda ()
      (eldoc-mode 1)
      (show-paren-mode 1)
      (/highlight-sexp-mode 1)
      (unless (/scratch-buffer-p)
      	(/company-mode-on
      	  (company-capf company-abbrev-code company-keywords)))
      )))

(font-lock-add-keywords
 'emacs-lisp-mode
 `(("(\\(/def\\(?:macro\\|un\\|generic\\|method\\)\\*?\\)\\_>\\s-*\\(\\(?:\\sw\\|\\s_\\)+\\)?"
    (1 font-lock-keyword-face)
    (2 font-lock-function-name-face nil t))
   ("(\\(/def\\(?:var\\|const\\|custom\\)\\*?\\)\\_>\\s-*\\(\\(?:\\sw\\|\\s_\\)+\\)?"
    (1 font-lock-keyword-face)
    (2 font-lock-variable-name-face nil t))
   ("(\\(/defclass\\*?\\)\\_>\\s-*\\(\\(?:\\sw\\|\\s_\\)+\\)?"
    (1 font-lock-keyword-face)
    (2 font-lock-type-face nil t))
   ))

(/provide)
;;; config/emacs-lisp.el ends here
