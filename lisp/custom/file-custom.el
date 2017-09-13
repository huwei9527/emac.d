;; -*- lexical-binding : t byte-compile-dynamic : t -*-

(eval-when-compile (require 'file-code))

(code-defilter-tail "file" "(~|#)|(.(exe|zip|pdf))")

(defconst file-custom-read-only-file-list
  `("~/Projects/emacs.d/test1.txt")
  "Read only file list. When open file in the list, set the buffer read only
automatically.")

(defconst file-custom-read-only-directory-list
  `(,config-packages-directory)
  "Read only directory list. When open file in these directory, set the buffer
read only automatically.")

(defvar file-custom-big-file-line-threshold (* 1024 5))
(defvar file-custom-big-file-size-threshold (* file-custom-big-file-line-threshold 64))

(defvar file-custom-path-max 2048
  "Max length of path (url, file path, etc) string.")

(defvar file-custom-invalid-path-char
  '(?\" ?\' ?\( ?\) ?\< ?\> ?\[ ?\] ?\` ?\{ ?\} ?\C-?)
  "Invalid path constituent character.")

(provide 'file-custom)
; file-custom ends here
