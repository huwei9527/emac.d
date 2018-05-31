;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-


(defun python-main-file (&optional root)
  ""
  (expand-file-name "main.py" root))

(defun python-makefile-file (&optional root)
  ""
  (expand-file-name "Makefile" root))

(defun python-compile-file (file)
  ""
  (let* ((buf (find-file-noselect file)))
    (with-current-buffer buf
      (call-interactively 'elpy-shell-send-region-or-buffer))))

(defun python-super-compile ()
  ""
  (interactive)
  (let* ((dir (ffip-project-root))
	 (main-file (python-main-file dir)))
    (if (file-exists-p main-file)
	(python-compile-file main-file)
      (call-interactively 'elpy-shell-send-region-or-buffer))))

(provide 'python-lib)
;;; python-lib.el ends here
