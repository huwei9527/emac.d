;;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

;;; Commentary:

;;; Code:

(defun /company-newline ()
  "Abort company-complete and insert a new line."
  (interactive)
  (company-abort)
  (newline-and-indent))

(/provide)
;;; lib/company.el ends here
