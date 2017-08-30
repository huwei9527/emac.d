;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

(require 'miscellany-custom)
(require 'file-lib)

;; Don't use lock files. (Don't create .#xxxx file)
(setq create-lockfiles nil)

;; Repeat type C-SPC after C-u C-SPC will cycle the mark ring.
(setq set-mark-command-repeat-pop t)

;; Set the default abbrev file.
(setq abbrev-file-name (expand-file-name config-abbrev-directory))

;; Do not allow duplicated item.
(setq history-delete-duplicates t)

; Read only file.
(add-hook 'find-file-hook
          (lambda (&rest)
            "Automatically set files in list read-only."
            (catch 'tag-break
              (when (file-custom-file-tail-filtered-p (buffer-file-name))
                (read-only-mode 1)
                (throw 'tag-break nil))
              (dolist (dir file-custom-read-only-directory-list)
                (when (file-in-directory-p buffer-file-name dir)
                  (read-only-mode 1)
                  (throw 'tag-break nil)))
              (dolist (fn file-custom-read-only-file-list)
                (when (string= buffer-file-name (expand-file-name fn))
                  (read-only-mode 1)
                  (throw 'tag-break nil))))))

(setq command-error-function
      (lambda (data context caller)
        "Ignore 'text-read-only' error in minibuffer."
        (unless (and (eq (car data) 'text-read-only) (minibufferp))
          (command-error-default-function data context caller))))

;; (which-func-mode 1)
;; (display-time-mode 1)

(provide 'config-miscellany)
; config-miscellany.el ends here