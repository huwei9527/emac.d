;;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

;;; Commentary:

;;; Code:

; (require 'init/graphic)
(setq inhibit-startup-screen t)
;; (require '/init/global
;; 	 (expand-file-name "lisp/init/global.el" user-emacs-directory))

(/require-meta file)
(switch-to-buffer "*Messages*")

(provide '/init/terminal)
;;; init/terminal.el ends here
