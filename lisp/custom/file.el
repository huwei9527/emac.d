;;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (/require-meta file)
  (/require-meta core))

(/require-lib core)

(/def-user-directory '/--lisp-name "User code directory.")
(/def-user-directory '/--config-name "User configuration file directory.")

(/def-file-name-regexp-all)

(/def-custom-var read-only-file-list
  `("~/Projects/emacs.d/test1.txt")
  "Read only file list.
When Emacs opens a file in this list, the corresponding buffer is set
  read only initially.")

(/def-custom-var read-only-directory-list
  `(,/custom-packages-directory
    "~/Codes"
    "~/.local/lib")
  "Read only file directory list. 
When Emacs opens a file in these directory, the corresponding buffer is
  set read only initially.")

(/def-custom-var big-file-line-threshold (* 1024 5)
  "Line threshold over which the `linum-mode' is turned off." 1)
(/def-custom-var big-file-size-threshold
  (* /custom-big-file-line-threshold 64)
  "File size threshold over which the `linum-mode' is turned off." 1)

(/def-config-directory abbrev "e.g,  abbrev_defs")
(/def-config-directory saveplace "e.g, place")
(/def-config-directory smex "e.g, smex-items")
(/def-config-directory ido "e.g., id.last")
(/def-config-directory recentf "e.g. recentf")
(/def-config-directory custom
  "Emacs custom file directory.
Store the custom file used by the `customize*' command.
See `custom-file' for details.")

(/def-custom-var recentf-exclude-list
  (/regexp-quote ".el.gz'"
		 ".(log|aux|rip)'"
		 "`_"
		 "__init__"
		 (expand-file-name "~/Codes")
		 (expand-file-name "~/.local/lib")
		 /custom-packages-directory
		 (expand-file-name "packages" user-emacs-directory)
		 ))


(/def-custom-var max-path-length 2048 "The max length of path string.")
(/def-custom-var invalid-path-char-list
  '(?\" ?\' ?\` ?\C-?
	?\( ?\)
	?\< ?\>
	?\[ ?\]
	?\{ ?\})
  "Invalid path constituest character.")

(/provide)
;;; custom/file.el ends here
