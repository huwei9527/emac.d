;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

;; (require '/init/global
;; 	 (expand-file-name "lisp/init/global.el" user-emacs-directory))

;; Place before configuration custom require to record the variable.
(defvar /--pre-create-directory-list nil)

(/require-custom
  elpa
  yasnippet
  company
  spell
  auto-save
  file
  ui
  miscellany
  theme
  )

(/require-lib file)
(dolist (dir /--pre-create-directory-list) (/make-directory-safe dir 'verbose))

(/provide)
;;; tool/create-config-directory.el ends here
