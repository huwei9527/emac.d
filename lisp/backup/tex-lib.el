;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

(require 'tex-custom)
(require 'util-lib)

(defun tex-temporary-buffer-p (&optional buf)
  "Return non-nil if BUF is TeX temporary buffer"
  (or buf (setq buf (current-buffer)))
  (with-current-buffer buf
    (or (eq major-mode 'TeX-error-overview-mode)
	(tex-help-buffer-p buf))))

(defun tex-help-buffer-p (&optional buf)
  "Return non-nil if BUF is \"*TeX Help*\""
  (or buf (setq buf (current-buffer)))
  (string= (buffer-name buf) "*TeX Help*"))

(defun tex-close-other-window ()
  "Close other window in TeX mode"
  (save-selected-window
    (let* ((win-live (window-list-1))
	   (win-other (next-window))
	   win-curr closed)
      (while win-live
	(setq win-curr (car win-live))
	(when (and (close-window 'tex-temporary-buffer-p
				 'first win-curr)
		   (eq win-curr win-other))
	  (setq closed t))
	(setq win-live (cdr win-live)))
      closed)))

(defun tex-switch-to-other-window ()
  ""
  nil)

(defun tex-exist-live-temporary-buffer-p ()
  "Return non-nil if there exist tex live temporary buffer."
  (let* ((win-live (window-list-1))
	 exist)
    (catch 'tag-exist
      (while win-live
	(when (tex-temporary-buffer-p (window-buffer (car win-live)))
	  (setq exist t)
	  (throw 'tag-exist t))
	(setq win-live (cdr win-live))))
    exist))

(defun tex-close-window-after-compilation-finished (&rest args)
  "Close the window create by compilation if it finished successful"
  (when (and tex-custom-window (not (one-window-p))
	     (not (tex-exist-live-temporary-buffer-p)))
    (delete-other-windows tex-custom-window)))

(defun tex-close-window-after-tex-command-advice (&rest args)
  "Record the window layout before tex comilation"
  (if (one-window-p)
      (setq tex-custom-window (selected-window))
    (setq tex-custom-window nil)))

(provide 'tex-lib)
;;; tex-lib.el ends here
