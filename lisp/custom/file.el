;;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

;;; Commentary:

;;; Code:

(eval-when-compile (/require-meta core file))

(/require-lib core)

(/--define-regexp)

(/defcustom read-only-file-list
  `("~/Projects/emacs.d/test1.txt")
  "Read only file list.
When Emacs opens a file in this list, the buffer is set read only.")

(/defcustom read-only-directory-list
  `(,/custom-packages-directory
    "~/Codes"
    "~/.local/lib")
  "Read only file directory list. 
When Emacs opens a file in these directories, the buffer is set read only.")

(/defcustom big-file-line-threshold (* 1024 5)
  "Line threshold over which the `linum-mode' is turned off.")
(/defcustom big-file-size-threshold
  (* /custom-big-file-line-threshold 64)
  "File size threshold over which the `linum-mode' is turned off.")

(/define-config-directory abbrev "e.g,  abbrev_defs")
(/define-config-directory saveplace "e.g, place")
(/define-config-directory smex "e.g, smex-items")
(/define-config-directory ido "e.g., id.last")
(/define-config-directory recentf "e.g. recentf")
(/define-config-directory custom
  "Emacs custom file directory.
Store the custom file used by the `customize*' command.
See `custom-file' for details.")

(/defcustom recentf-exclude-list
  (/regexp-quote ".el.gz'"
		 ".(log|aux|rip)'"
		 "`_"
		 "__init__"
		 (expand-file-name "~/Codes")
		 (expand-file-name "~/.local/lib")
		 (expand-file-name "~/.local/share/texlive")
		 (expand-file-name "/usr/share/emacs")
		 /custom-packages-directory
		 (expand-file-name "packages" user-emacs-directory)
		 ))


(/defcustom max-path-length 2048 "The max length of path string.")
(/defcustom invalid-path-char-list
  '(?\" ?\' ?\` ?\C-?
	?\( ?\)
	?\< ?\>
	?\[ ?\]
	?\{ ?\})
  "Invalid path constituest character.")

(/provide)
;;; custom/file.el ends here
