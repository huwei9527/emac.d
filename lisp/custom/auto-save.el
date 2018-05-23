;;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

;;; Commentary:

;;; Code:

(eval-and-compile (/require-meta file))

(/def-config-directory backup
  "Emacs backup file directory. (filename.~1~, filename.~2~, ...)")

(/def-config-directory auto-save
  "Emacs auto-save file directory. (#filename#)")

(/def-config-directory auto-save-alist
  "The directory to save Emacs recover file. (uid-username~)")

(/provide)
;;; custom/auto-save.el ends here
