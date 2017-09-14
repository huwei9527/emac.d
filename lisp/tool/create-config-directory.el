;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

;; Place before configuration custom require to record the variable.
(setq config-directory-list nil)

;; Must before any configuration package.
(require 'global-custom
         ;; Specify the file because load-path is set after it is loaded.
         (expand-file-name "lisp/custom/global-custom.el" user-emacs-directory))


(require 'elpa-custom)
(require 'color-theme-custom)
(require 'auto-save-custom)
(require 'file-lib)
(require 'company-custom)
(require 'miscellany-custom)
(require 'desktop-custom)

(dolist (dir config-directory-list) (make-directory-safe (symbol-value dir)))

(provide 'create-config-directory)
; create-config-directory.el ends here
