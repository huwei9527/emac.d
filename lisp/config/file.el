;;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

;;; Commentary:

;;; Code:

(eval-when-compile (/require-meta file hook))

(/require-custom file)
(/require-lib file)

(setq confirm-nonexistent-file-or-buffer t ;; always confirm new file
      ;; abbrev
      abbrev-file-name (/config-file-path abbrev_defs abbrev)
      ;; save-place
      save-place-file (/config-file-path places saveplace)
      ;; smex
      smex-save-file (/config-file-path smex-items smex)
      ;; ido
      ido-save-directory-list-file (/config-file-path ido.last ido)
      ;; recentf
      recentf-save-file (/config-file-path recentf)
      recentf-max-saved-items 1024
      recentf-exclude /custom-recentf-exclude-list
      ;; emacs c source file
      source-directory (file-truename (expand-file-name "~/Codes/emacs"))
      ;; custom file
      custom-file (/config-file-path custom.el custom)
      )

;; load custom file
(if (file-exists-p custom-file) (load custom-file))

;; save cursor place
(save-place-mode 1)

;; Read only file
(/add-hook (find-file-hook)
  (lambda (&rest args)
    "Set buffer read-only."
    (if buffer-file-name
	(catch 'break-tag
	  (when (/uneditable-file-p buffer-file-name)
	    (read-only-mode 1)
	    (throw 'break-tag nil))
	  (dolist (file /custom-read-only-file-list)
	    (when (string= buffer-file-name (expand-file-name file))
	      (read-only-mode 1)
	      (throw 'break-tag nil)))
	  (dolist (dir /custom-read-only-directory-list)
	    (when (/file-in-directory-p buffer-file-name dir)
	      (read-only-mode 1)
	      (throw 'break-tag nil)))))))

(/provide)
;;; config/file.el ends here
