;;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (/require-meta file)
  (/require-meta core))

(/def-user-directory (eval /lisp-name) "User code directory.")
(/def-user-directory (eval /config-name)
  "The directory for user configuration files.")

(/def-custom-var dotdirectory-regexp
  (format "\\`%s\\'" (regexp-opt (list "." "..")))
  "Regexp for system '.' and '..' directory." 'eval)

(/def-custom-var testdir "~/Projects/test/abcd" "AAA")

(/provide)
;;; custom/file.el ends here
