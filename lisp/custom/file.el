;;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

;;; Commentary:

;;; Code:

(/def-user-directory (eval /lisp-name) "User code directory.")
(/def-user-directory (eval /config-name)
  "The directory for user configuration files.")

(provide '/custom/file)
;;; custom/file.el ends here
