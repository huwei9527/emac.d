;;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

;;; Commentary:

;;; Code:

(defun /python-compile ()
  "Run python code."
  (interactive)
  (call-interactively #'elpy-shell-send-region-or-buffer))

(/provide)
;;; lib/python.el ends here
