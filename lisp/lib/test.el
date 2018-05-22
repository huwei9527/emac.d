;;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

;;; Commentary:

;;; Code:

(defun /message-test-start ()
  ""
  (message "In %s: test start." load-file-name))

(defun /message-test-end ()
  ""
  (message "In %s: test end." load-file-name))

(/provide)
;;; lib/test.el ends here
