;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

(eval-when-compile (require 'file-code))

(defconst elpa-custom-packages-list
  '(
    ;; import vim in emacs
    evil
    ;; color-theme-solarized
    ;; zenburn-theme
    ;; labburn-theme
    ;; molokai-theme
    ;; ivy-hydra
    ;; fuzzy search sortting
    flx
    ;; ivy minibuffer completion and counsel utils
    counsel
    ;; show keymap menu
    which-key
    ;; find file in project like ctrl-p
    find-file-in-project
    ;; latex environment
    auctex
    ;; paredit
    paredit
    paredit-everywhere
    rainbow-delimiters
    hl-sexp
    ;; hideshow-org
    ;; hideshowvis
    company
    company-statistics
    browse-kill-ring+
    )
  "Package to be install.")

(code-defdir "packages"
  "The directory to store ELPA packages. This will be set to
package-user-dir automatically.")

(code-defdir-config "package-describes"
  "The directory to save ELPA describe file. When you describe a
package (C-m or <RET> in the package mode) which is no installed,
ELPA will download a package-name-readme.txt file to describe the
package to you. The file is store in the package-user-dir directory
by default which would dirty the directory. So I hack it to this
directory by advice describe-package-1.")


(provide 'elpa-custom)
; elpa-custom.el ends here
