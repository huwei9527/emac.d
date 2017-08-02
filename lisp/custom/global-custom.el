;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-
(eval-when-compile
  (require 'file-code
           (expand-file-name "lisp/lib/code/file-code.el" user-emacs-directory)))
(require 'path-lib
         (expand-file-name "lisp/lib/path-lib.el" user-emacs-directory))

;; Customize directory
(defgroup config-custom nil
  "Config file structure."
  :group 'convenience
  :prefix "config-")

;; Define config-lisp-directory, config-lisp-directory-name
(code-defdir "lisp"
  "The directory where stores emacs config source files.
user-emacs-directory/lisp.")


;; Set the load-path for configuration lisp.
(add-directory-to-list config-lisp-directory 'load-path)

;; Define config-directory, config-directory-name.
(code-defdir nil
  "The directory to store emacs config files.
e.g. backup files, custom color theme...")

(provide 'global-custom)
; global-custom.el ends here