;;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

;;; Commentary:

;;; Code:

(/require-meta core)

(defmacro /company-mode-on (&rest backends)
  "Start company mode and setup buffer local BACKENDS."
  (declare (indent defun))
  (/--sexp-progn-exec
    `(company-mode 1)
    `(set (make-local-variable 'company-backends) ',backends)))

(/provide)
;;; meta/company.el ends here
