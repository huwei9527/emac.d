;;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

;;; Commentary:

;;; Code:

(/require-custom auto-save)

;;; Backup & autosave
;;    Default backup directory.
(setq backup-directory-alist `((".*" . ,/custom-config-backup-directory))
      ;; Default autosave directory.
      auto-save-file-name-transforms
      `((".*" ,/custom-config-auto-save-directory t))
      ;; Default recover-file directory.
      auto-save-list-file-prefix /custom-config-auto-save-alist-directory
      backup-by-copying t ; Make a new file as backup.
      save-silently t
      version-control t
      kept-new-versions 8
      kept-old-versions 2
      delete-old-versions t
      ;; Only backup file which Emacs can edit.
      backup-enable-predicate
      (lambda (name)
        (and (normal-backup-enable-predicate name)
             (not (/uneditable-file-p name)))))

(/provide)
;;; config/auto-save.el ends here
