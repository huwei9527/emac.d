;;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

;;; Commentary:

;;; Code:

(eval-when-compile (/require-meta hook keymap))

(/require-custom ffip)

(/def-keys-ctl-x
 f find-file-in-project-by-selected)

;; Don't search home directory
(/advice-add (ffip-project-root) :around
  (lambda (fun &rest args)
    (let* ((rlt (apply fun args)))
      (catch 'break-tag
	(dolist (dir /custom-ffip-large-directory-list)
	  (when (file-equal-p default-directory dir)
	    (setq rlt /custom-ffip-default-directory)
	    (throw 'break-tag nil))))
      rlt)))

(/provide)
;;; config/ffip.el ends here
