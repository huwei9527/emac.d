;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

;;; Commentary:

;;; Code:

;; (require '/init/global
;; 	 (expand-file-name "lisp/init/global.el" user-emacs-directory))


(/require-config elpa)
(/require-lib elpa format)

(/package-install)

(/provide)
;;; tool/install-packages.el ends here
