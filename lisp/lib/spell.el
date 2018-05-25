;;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

;;; Commentary:

;;; Code:

(/require-custom spell)
(/require-lib file)

(defun /spell-set-personal-dictionary ()
    "Set the buffer local personal dictionary `ispell-local-pdict'."
    (let* ((bufname (/file-or-buffer-name)) name attrs)
      ;; search in regexp alist, frist match.
      (catch 'break-tag
	(dolist (attrs /custom-spell-personal-regexp-alist)
	  (when (string-match-p (car attrs) bufname)
	    (or (setq name (cdr attrs)) (setq name (car attrs)))
	    (throw 'break-tag nil))))
      ;; search in mode alist
      (or name
	  (and (setq attrs (assoc major-mode
				  /custom-spell-personal-mode-alist))
	       (or (setq name (cdr attrs)) (setq name (car attrs)))))
      ;; construct the personal filename in the configure directory
      (and name
	   (setq ispell-local-pdict
		 (expand-file-name
		  (format "%s%s"
			  (expand-file-name
			   (/path-to-file-name (/--name name))
			   /custom-config-spell-directory)
			  /custom-spell-personal-file-extension))))))

(defun /spell-restart ()
  "Restart Ispell process"
  (interactive)
  (ispell-kill-ispell t)
  (ispell-init-process))

(/provide)
;;; lib/spell.el ends here
