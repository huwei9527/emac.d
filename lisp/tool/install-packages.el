;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

;;; Commentary:

;;; Code:

;; (require '/init/global
;; 	 (expand-file-name "lisp/init/global.el" user-emacs-directory))

(setq install-packages-start-time (current-time))

(/require-config elpa)
(/require-lib elpa format)

(package-refresh-contents)
(/package-install)

(message "Time: %f s."
         (time-to-seconds (time-since install-packages-start-time)))

(/provide)
;;; tool/install-packages.el ends here
