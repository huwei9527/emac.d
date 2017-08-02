;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

(defvar ui-custom-inhibit-linum-mode-list
  (list 'help-mode
        'Info-mode
        'completion-list-mode
        'compilation-mode
        )
  "Major mode list which disable line number.")

(provide 'ui-custom)
; ui-custom.el ends here