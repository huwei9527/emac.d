(require 'config-custom)

;; Backup & autosave
; Default backup directory.
(setq backup-directory-alist `((".*" . ,config-backup-directory)))
; Default autosave directory.
(setq auto-save-file-name-transforms `((".*" ,config-auto-save-directory t)))
; Default recover-file directory.
(setq auto-save-list-file-prefix config-auto-save-alist-directory)
(setq backup-by-copying t ; Make a new file as backup.
      version-control t
      kept-new-versions 8
      kept-old-versions 2
      delete-old-versions t)
; Only backup file which Emacs can edit.
(setq backup-enable-predicate
      (lambda (name)
        (and (normal-backup-enable-predicate name)
             (not (string-match-p config-file-filter-regexp name)))))

;(setq auto-save-interval 5)
(require 'smart-auto-save)

(provide 'config-auto-save)
; config-auto-save.el ends here