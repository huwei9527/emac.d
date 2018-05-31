;;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

;;; Commentary:

;;; Code:

(setq emacs-load-start-time (current-time))

;; Must before any configuration package.
(require '/init/global
	 (expand-file-name "lisp/init/global.el" user-emacs-directory))

(if noninteractive
    (cond
     ((boundp '*create*) (/require-tool create-config-directory))
     ((boundp '*install*) (/require-tool install-packages))
     ((boundp '*test*) (/require-test test))
     (t (message "Exit.")))
  (if (display-graphic-p) (/require-init graphic) (/require-init terminal)))

(defvar emacs-load-time (time-to-seconds (time-since emacs-load-start-time)))
(message "%f" emacs-load-time)
(setq initial-scratch-message (format ";; %f\n" emacs-load-time))

(provide 'init)

;;; init.el ends here
