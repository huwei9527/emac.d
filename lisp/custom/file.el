;;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (/require-meta file)
  (/require-meta core))

(/require-lib core)

(/def-user-directory '/--lisp-name "User code directory.")
(/def-user-directory '/--config-name "User configuration file directory.")

(/def-file-name-regexp-all)

(/def-custom-var read-only-file-list
  `("~/Projects/emacs.d/test1.txt")
  "Read only file list.
When Emacs opens a file in this list, the corresponding buffer is set
  read only initially.")

(/def-custom-var read-only-directory-list
  `(,/custom-packages-directory
    "~/Codes"
    "~/.local/lib")
  "Read only file directory list. 
When Emacs opens a file in these directory, the corresponding buffer is
  set read only initially.")

(/def-custom-var big-file-line-threshold (* 1024 5)
  "Line threshold over which the `linum-mode' is turned off." 1)
(/def-custom-var big-file-size-threshold
  (* /custom-big-file-line-threshold 64)
  "File size threshold over which the `linum-mode' is turned off." 1)

(/def-custom-var max-path-length 2048 "The max length of path string.")
(/def-custom-var invalid-path-char
  '(?\" ?\' ?\` ?\C-?
	?\( ?\)
	?\< ?\>
	?\[ ?\]
	?\{ ?\})
  "Invalid path constituest character.")


(/provide)
;;; custom/file.el ends here
