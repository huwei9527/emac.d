;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

(require 'auto-save-custom)
(require 'smart-auto-save)
(require 'file-lib)

      ;; Backup & autosave
      ;; Default backup directory.
(setq backup-directory-alist `((".*" . ,config-backup-directory))
      ;; Default autosave directory.
      auto-save-file-name-transforms `((".*" ,config-auto-save-directory t))
      ;; Default recover-file directory.
      auto-save-list-file-prefix config-auto-save-alist-directory
      backup-by-copying t ; Make a new file as backup.
      version-control t
      kept-new-versions 8
      kept-old-versions 2
      delete-old-versions t
      ;; Only backup file which Emacs can edit.
      backup-enable-predicate
      (lambda (name)
        (and (normal-backup-enable-predicate name)
             (not (file-custom-file-tail-filtered-p name)))))

;; Enable smart-auto-save:
;; save when idle, switch-buffer, switch window, frame-out.
(smart-auto-save-on)

(provide 'config-auto-save)
; config-auto-save.el ends here