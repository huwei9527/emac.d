;;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

;;; Commentary:

;;; Code:

(/require-lib file)

(defvar /testdir "~/Projects/test/abcd" "Doc testdir")

(defun /test-editable (path)
  "Test editable."
  (message "%s : %s" (/uneditable-file-p path) path))

(print (/subdirectory /testdir))
(/test-editable "/a/b/c.pdf")
(/test-editable "/a/b/c.exe")
(/test-editable "/a/b/c.zip")
(/test-editable "/a/b/c.pdfaaa")
(/test-editable "/a/b/c.exeaaa")
(/test-editable "/a/b/c.zipaaa")
(/test-editable "/a/b/c.ex~e")
(/test-editable "/a/b/c.exe~")
(/test-editable "/a/b/c.exe#")
(/test-editable "/a/b#/cafdaf")
(print (/path-to-file-name "a/b/c/d"))

; (/ppmacroexpand (/def-file-name-regexp-all))

(/provide)
;;; test/file.el ends here
