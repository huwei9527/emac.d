;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

(setq install-packages-start-time (current-time))

;; Must before any configuration package.
(require 'global-custom
         ;; Specify the file because load-path is set after it is loaded.
         (expand-file-name "lisp/custom/global-custom.el" user-emacs-directory))

(require 'config-elpa)
(require 'elpa-lib)

(install-package)

(message "Time: %f s."
         (time-to-seconds (time-since install-packages-start-time)))

(provide 'install-packages)
; install-packages.el ends here
