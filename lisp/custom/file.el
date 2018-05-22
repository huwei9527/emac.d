;;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (/require-meta file)
  (/require-meta core))

(/def-user-directory '/--lisp-name "User code directory.")
(/def-user-directory '/--config-name "User configuration file directory.")

(/def-custom-var dotdirectory-regexp
  (format "\\`%s\\'" (regexp-opt (list "." "..")))
  "Regexp for system '.' and '..' directory." 1)


(/provide)
;;; custom/file.el ends here
