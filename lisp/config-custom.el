(unless (boundp 'config-home-directory)
  (message "Invalid home directory."))

;; Customize directory
(defgroup config-custom nil
  "Config file structure."
  :group 'convenience
  :prefix "config-")

(defcustom config-name "config"
  "Config directory name."
  :type 'string
  :group 'config-custom)

(defcustom config-backup-name "backup"
  "backup directory name."
  :type 'string
  :group 'config-custom)

(defcustom config-auto-save-alist-name "auto-save-alist"
  "Auto-save-alist directory name."
  :type 'string
  :group 'config-custom)

(defcustom config-auto-save-name "auto-save"
  "Auto-save directory name."
  :type 'string
  :group 'config-custom)

(defcustom config-packages-name "packages"
  "ELPA packages directory name."
  :type 'string
  :group 'config-custom)

(defcustom config-lisp-name "lisp"
  "Config lisp directory name."
  :type 'string
  :group 'config-custom)

(defcustom config-file-filter "(~|#)|(.(exe|zip|pdf))"
  "File type filer"
  :type 'string
  :group 'config-custom)

;; Construct directory by custom variable.
(defconst config-directory
  (file-name-as-directory (expand-file-name config-name config-home-directory)))
(defconst config-backup-directory
  (file-name-as-directory (expand-file-name config-backup-name config-directory)))
(defconst config-auto-save-directory
  (file-name-as-directory (expand-file-name config-auto-save-name config-directory)))
(defconst config-auto-save-alist-directory
  (file-name-as-directory (expand-file-name config-auto-save-alist-name config-directory)))
(defconst config-packages-directory
  (file-name-as-directory (expand-file-name config-packages-name config-home-directory)))
(defconst config-lisp-directory
  (file-name-as-directory (expand-file-name config-lisp-name config-home-directory)))
(defconst config-file-filter-regexp
  (concat (replace-regexp-in-string "[()|.]" "\\\\\\&" config-file-filter) "\\'"))

;; Load path
(dolist (fn (directory-files config-lisp-directory))
  (unless (string-match-p "\\`\\(\\.\\|\\.\\.\\)\\'" fn)
    (setq fn (expand-file-name fn config-lisp-directory))
    (if (file-directory-p fn) (add-to-list 'load-path fn))))

(provide 'config-custom)
; config-custom.el ends here