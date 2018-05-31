;;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

;;; Commentary:

;;; Code:

(eval-when-compile (/require-meta file core))

(/def-custom-var package-archives
  `(("gnu"   . "https://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
    ("melpa" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/"))
  "An alist of achives from which to fetch.
This will feed to `package-archives'.")

(/def-custom-var package-alist
  `((evil . t)			; vim
    (color-theme-solarized)	; solarized color theme
    (zenburn-theme)		; zenburn color theme
    (labburn-theme)		; labburn color theme
    (molokai-theme)		; molokai color theme
    (flx . t)			; fuzzy search sortting
    (smex . t)			; M-x-mru
    (counsel . t)		; minibuffer completion
    (ivy-hydra)			; minibuffer completion control panel
    (which-key . t)		; show keymap interactively
    (find-file-in-project . t)	; find in project
    (paredit . t)		; auto pair sexp
    (paredit-everywhere . t)	; auto pair sexp-like object
    (yasnippet . t)		; snippet
    (rainbow-delimiters . t)	; colored parenthesis
    (hideshow-org)		; org-mode hideshow
    ;(hideshowvis)		; hideshow +/- icons
    (company . t)		; completion engine
    (company-statistics . t)	; complete-mru
    (company-ycmd . t)		; completion server
    (browse-kill-ring . t)	; kill ring
    (workgroups)		; save session
    (workgroups2)		; save session
    (ace-jump-mode . t)		; move cursor
    (ace-link . t)		; open link
    (flyspell-lazy . t)		; spell check in idle time
    (popwin) 			; pop window
    (elpy . t)			; python major mode
    (auctex . t)		; TeX major mode
    (company-auctex . t)	; completion for TeX
    (magit . t)			; git tool
    (disable-mouse . t)		; diable mouse click
    )
  "The alist of user installed packages and select which one to load.
Each element is a concell. 
The first element is the name of the package (a symbol). 
The second element is the version.
If version is t, the package is loaded with the newest version.
If version is a string, only that version is loaded.
If version is nil, the package is not loaded (installed but disabled).
See `package-load-list' for more explanation.")

(/def-user-directory packages
  "The directory to store ELPA packages. This will be set to
package-user-dir automatically.")

(/def-config-directory package-describes
  "The directory to save ELPA describe file. When you describe a
package (C-m or <RET> in the package mode) which is no installed,
ELPA will download a package-name-readme.txt file to describe the
package to you. The file is store in the package-user-dir directory
by default which would dirty the directory. So I hack it to this
directory by advice describe-package-1.")

(/provide)
;;; custom/elpa.el ends here
