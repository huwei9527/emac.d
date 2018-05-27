;;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

;;; Commentary:

;;; Code:

(/require-lib file)

(/message-test-start)
(defvar /testdir "~/Projects/test/abcd" "Doc testdir")

(defun /test-editable (path)
  "Test editable."
  (message "%s : %s" (/uneditable-file-p path) path))

(when nil
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
  (print (/path-to-file-name "a/b/c/d")))

; (/ppmacroexpand (/def-file-name-regexp-all))

(when t
  (let* ((cnt 0))
    (when nil
      (/test-char "\\" /char-path-delimiter-p)
      (/test-char "/" /char-path-delimiter-p)
      (/test-char a /char-path-delimiter-p)
      (/test-char ?1 /char-path-delimiter-p)
      (/test-char - /char-path-delimiter-p)
      )
    (when t
      (/test-char a /char-path-p)
      (/test-char " " /char-path-p)
      (/test-char "<" /char-path-p)
      (/test-char "(" /char-path-p)
      )
    ))

(/message-test-end)

(/provide)
;;; test/file.el ends here
