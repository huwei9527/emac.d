;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-
(eval-when-compile
  (require 'code-gen
           (expand-file-name "lisp/lib/code-gen.el" user-emacs-directory)))
(require 'path-lib
         (expand-file-name "lisp/lib/path-lib.el" user-emacs-directory))

;; Customize directory
(defgroup config-custom nil
  "Config file structure."
  :group 'convenience
  :prefix "config-")

;; Define config-lisp-directory, config-lisp-directory-name
(code-gen-defdir "lisp"
  "The directory where stores emacs config source files.
user-emacs-directory/lisp.")


;; Set the load-path for configuration lisp.
(add-directory-to-list config-lisp-directory 'load-path)

;; Define config-directory, config-directory-name.
(code-gen-defdir nil
  "The directory to store emacs config files.
e.g. backup files, custom color theme...")

(provide 'global-custom)
; global-custom.el ends here