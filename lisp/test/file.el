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
  (/defregexp abcd "`abcd(e|f)" "xxxx" 'quote)
  (/define-file-name-predictor abcd "nil")
  (message "%s" (/abcd-file-name-p "/a/b/c/abcdexxx"))
  (message "dotdirectory: %s" (/dotdirectory-file-name-p "/a/b/c/.."))
  (message "dotdirectory: %s" (/dotdirectory-file-name-p "/a/b/c/..."))
  (message "uneditable:   %s" (/uneditable-file-file-name-p "/a/b/c/d~"))
  (message "system:       %s" (/system-buffer-file-name-p "/a/b/c/*abc"))
  (message "scratch:      %s" (/scratch-buffer-file-name-p "/a/b/c/*scratch*"))
  (message "message:      %s" (/message-buffer-file-name-p "/a/b/c/*Messages*")))

(when nil
  (message "%s" (/path '/a 'b 'c 'd))
  (message "%s" (/path 'a 'b 'c 'd))
  (message "%s" (/path 12))
  (message "%s" (/path))
  (message "%s" (/path-to-file-name "/a/b/c/d"))
  (message "%s" (/path-to-file-name default-directory)))

(when nil
  (message "%s" (/file-contain-p "/a/b/c" "/a/b"))
  (message "%s" (/file-contain-p "/a/d/c" "/a/b"))
  (message "%s" (/file-name-match "/a/b/c/xxxxyyyxxx" "yyy"))
  )

(defvar list '("XXX" "YYY"))

(when t
  (let* ((path "~/Projects/test/abcd")
	 (list nil))
    (message "%s" (/subdirectory path))
    (message "%s" (/subdirectory path 1))
    ;(add-to-list 'list "aaa")
    (/add-subdirectory-to-list path 'list)
    (message "%s" list)
    )
  (message "%s" list)
  )

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

(when nil
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

(when nil
  (/make-directory-safe "~/Projects/test/xxxxx" 'verbose))

(/message-test-end)

(/provide)
;;; test/file.el ends here
