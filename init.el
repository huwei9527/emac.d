;;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

;;; Commentary:

;;; Code:

(setq /emacs-start-time (current-time))

;; Must before any configuration package.
(eval-when-compile
  (require '/init/global
	   (expand-file-name "lisp/init/global.el" user-emacs-directory)))

(if noninteractive
    (cond
     ((boundp '*create*) (/require-tool create-config-directory))
     ((boundp '*install*) (/require-tool install-packages))
     ((boundp '*test*) (/require-test test))
     (t (message "Exit.")))
  (if (display-graphic-p) (/require-init graphic) (/require-init terminal)
    ))

(defvar /emacs-load-time (float-time (time-since /emacs-start-time))
  "Emacs init file loading time.")
(message "Time: %f s" /emacs-load-time)
(setq initial-scratch-message (format ";; Load time: %f s.\n" /emacs-load-time))

(provide 'init)

;;; init.el ends here
