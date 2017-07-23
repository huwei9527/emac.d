(unless (boundp 'config-home-directory)
  (defconst config-home-directory
    (file-name-as-directory (expand-file-name "~/Projects/emacs.d"))))
(add-to-list 'load-path (file-name-as-directory (expand-file-name "lisp" config-home-directory)))

(require 'config-custom)

(defun make-config-directory (dir)
  (unless (file-directory-p dir)
    (make-directory dir)
    (message "Create directory (%s)." dir)))

(make-config-directory config-directory)
(make-config-directory config-backup-directory)
(make-config-directory config-auto-save-alist-directory)
(make-config-directory config-auto-save-directory)
(make-config-directory config-packages-directory)
(make-config-directory config-lisp-directory)

(provide 'create-config-directory)
; create-config-directory.el ends here
