;;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

;;; Commentary:

;;; Code:

(/require-meta ui)


(defvar /--face-list nil "Face list")
(defvar /fattrs (car /--color-alist) "For test.")
(defvar /battrs (cadr /--color-alist) "For test.")
(pp-macroexpand-expression '(/defface--single /fattrs))
(pp-macroexpand-expression '(/defface--single /fattrs 'fg))
(pp-macroexpand-expression '(/defface--double /fattrs /battrs))
(pp /--face-list)


(/provide)
;;; test/ui.el ends here
