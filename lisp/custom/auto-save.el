;;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

;;; Commentary:

;;; Code:

(eval-and-compile (/require-meta file))

(/define-config-directory backup
  "Emacs backup file directory. (filename.~1~, filename.~2~, ...)")

(/define-config-directory auto-save
  "Emacs auto-save file directory. (#filename#)")

(/define-config-directory auto-save-alist
  "The directory to save Emacs recover file. (uid-username~)")

(/defcustom auto-save-idle-time 1
  "The idle seconds to start auto save.")

(/defcustom auto-save-interval auto-save-interval
  "The interval between auto-save.")

(/defcustom auto-save-silently t
  "If non-nil, avoid messages when saving files.")

(/provide)
;;; custom/auto-save.el ends here
