;;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

;;; Commentary:

;;; Code:

(/require-lib elpa)
(/message-test-start)
(when t
  (let* ((attrs (assq 'evil package-alist))
	 (pkg (car attrs))
	 (name (symbol-name pkg))
	 (desc (cadr attrs))
	 (alist package-archive-contents))
    ;(message "%s" desc)
    (when nil
      (message "%s" (/--package-name name))
      (message "%s" (/--package-name pkg))
      (message "%s" (/--package-name desc))
      (message "%s" (/--package-desc name))
      (message "%s" (/--package-desc pkg))
      (message "%s" (/--package-desc desc))
      (message "%s" (/--package-desc pkg alist))
      )
    (when nil
      (message "%s" (/--package-dependence pkg))
      (message "%s" (/--package-dependence desc))
      (message "%s" (/--package-version pkg))
      (message "%s" (/--package-version desc alist))
      (message "%s" (/package-version pkg))
      (message "%s" (/package-remote-version desc))
      )
    (defun testfun (list) "" (push 100 list) (message "innser %s" list))
    (when nil
      (/package-install)
      )
    )
  )
(/message-test-end)

(/provide)
;;; .el ends here
