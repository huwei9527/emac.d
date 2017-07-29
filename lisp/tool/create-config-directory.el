;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

;; Must before any configuration package.
(require 'global-custom
         ;; Specify the file because load-path is set after it is loaded.
         (expand-file-name "lisp/custom/global-custom.el" user-emacs-directory))

;; Place before configuration custom require to record the variable.
(defvar config-directory-list nil
  "Directory list for directory configuration symbols.")

(require 'global-custom)
(require 'elpa-custom)
(require 'color-theme-custom)
(require 'auto-save-custom)

(dolist (dir config-directory-list) (make-directory-safe (symbol-value dir)))

(provide 'create-config-directory)
; create-config-directory.el ends here
