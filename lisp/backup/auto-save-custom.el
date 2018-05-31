;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

(eval-and-compile (require 'file-code))

(code-defdir-config "backup"
  "The directory to store Emacs backup files.
e.g. filename.~1~, filename.~2~")

(code-defdir-config "auto-save"
  "The directory to store Emacs auto-save files.
e.g. #filename#")

(code-defdir-config "auto-save-alist"
  "The directory to save Emacs recover file.
e.g. uid-username~")

(provide 'auto-save-custom)
; auto-save-custom.el ends here