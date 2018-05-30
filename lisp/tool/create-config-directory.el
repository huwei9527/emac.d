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
(/require-test format)
(message "boundp : %s" (boundp '*create*))
(message "length: %s" (length /--pre-create-directory-list))
;(message "length: %s" /--pre-create-directory-list)
(switch-to-buffer "*Messages*")

;(dolist (dir config-directory-list) (make-directory-safe (symbol-value dir)))

(/provide)
;;; tool/create-config-directory.el ends here
