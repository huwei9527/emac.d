;;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

;;; Commentary:

;;; Code:

(eval-when-compile (/require-meta core))

(defmacro /set-regex-plus-search (&rest cmds)
  "Set `ivy--regex-plus' search for command in CMDS"
  (/--sexp-progn
    (dolist (cmd cmds)
      (/--sexp-exec
	`(add-to-list 'ivy-re-builders-alist '(,cmd . ivy--regex-plus))))))

(/provide)
;;; meta/ivy.el ends here
