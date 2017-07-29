;; -*- lexical-binding : t byte-compile-dynamic : t -*-

(require 'file-custom)

(defun file-in-directory-p (file dir)
  "Whether FILE is in sub-directory of DIR."
  (string-prefix-p (file-name-as-directory (expand-file-name dir))
                   (expand-file-name file)))

(defun make-directory-safe (dir-name &optional silent)
  "Create direcotry of name DIR-NAME.

It won't singal error if failed (e.g. directory exists). Don't show message
when SILENT is no-nil."
  (let* ((str-info nil))
    (if (file-directory-p dir-name)
        (setq str-info (propertize "*" 'face '(:foreground "green")))
      (make-directory dir-name)
      (setq str-info (propertize "+" 'face '(:foreground "red"))))
    (unless silent
      (message "[%s] %s" str-info dir-name))))

(defun big-file-p ()
  "Whether current file is too big."
  (or (> (buffer-size) file-custom-big-file-size-threshold)
      (> (line-number-at-pos (point-max)) file-custombig-file-line-threshold)))

(provide 'file-lib)
; file-lib.el ends here