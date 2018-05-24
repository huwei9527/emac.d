;;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

;;; Commentary:

;;; Code:

(/require-custom spell)
(/require-lib file)

(defun /spell-set-personal-dictionary ()
    ""
    (let* ((bufname (/file-or-buffer-name)) name attrs)
      ;; search in regexp alist
      (catch 'break-tag
	(dolist (attrs /custom-spell-personal-regexp-alist)
	  (when (string-match-p (car attrs) bufname)
	    (or (setq name (cadr attrs))
		(setq name (/path-to-file-name (car attrs))))
	    (throw 'break-tag nil))))
      ;; search in mode alist
      (or name
	  (and (setq attrs (assoc major-mode
				  /custom-spell-personal-mode-alist))
	       (or (setq name (cdr attrs))
		   (setq name (/--name (car attrs))))))
      (if name
	  (setq name (format "%s%s" name
			     /custom-spell-personal-file-extension))
	(setq name /custom-spell-personal-default-file))
      (setq name (expand-file-name name /custom-config-spell-directory)
	    ispell-local-pdict name)))

(defun /spell-restart ()
  ""
  (interactive)
  (ispell-internal-change-dictionary)
  (ispell-init-process))

(/provide)
;;; lib/spell.el ends here
