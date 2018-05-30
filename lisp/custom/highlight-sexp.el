;;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

;;; Commentary:

;;; Code:

;(eval-when-compile (/require-meta core))

(defface /highlight-sexp
  '((((type tty))
     (:bold t))
    (((class color) (background light))
     (:background "lightgray"))
    (((class color) (background dark))
     (:background "gray10"))
    (t (:bold t)))
  "Face used to fontify the sexp you're looking at.")

(/provide)
;;; custom/highlight.el ends here
