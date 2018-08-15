;;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

;;; Commentary:

;;; Code:

(eval-when-compile (/require-meta core file))

(/define-user-directory ((lisp))
  "`/lisp' directory.
The directory of user lisp code files.
This is a directory of `lisp /--format' name in `user-emacs-directory'.")

(/define-user-directory ((config))
  "`/config' directory.
The directory of user config files.
This is a directory of `config /--format' name in `user-emacs-directory'.")

(/defvar* transient-minor-mode-mode-line-tag nil
  "The mode line tag for transient minor mode.")

(/provide)
;;; custom/core.el ends here
