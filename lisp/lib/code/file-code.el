;; -*- lexical-binding : t byte-compile-dynamic : t -*-

;; This package is used by global-custom before it sets up
;; the load-path. So 'require' packages not in the default
;; load-path will failed unless you explicitly specify the path.
;; e.g (require 'xxx-package package-path)

(defmacro code-defdir (dir-name doc &optional path)
  "Define a directory.

\(defconst config-[DIR-NAME]-directory PATH/DIR-NAME DOC)
DIR-NAME / DOC / PATH (a string or nil)"
  (let* ((str-name (or dir-name "config"))
         (str-path (or path user-emacs-directory))
         (sn-dir (if dir-name (format "config-%s-directory" dir-name)
                   "config-directory"))
         (sb-dir (intern sn-dir)))
    `(progn
       (defconst ,sb-dir
         ,(file-truename (file-name-as-directory
			  (expand-file-name str-name str-path)))
         ,(format "The directory path of %s.\n\n%s" str-name doc))
       (and (boundp 'config-directory-list)
         (setq config-directory-list (cons ',sb-dir config-directory-list))))))

(defmacro code-defdir-config (dir-name doc)
  "Define a config directory.

Just a wrapper for (code-defdir DIR-NAME DOC config-directory)"
  `(code-defdir ,dir-name ,doc ,config-directory))

(defmacro code-defilter-tail (name str-filter)
  "Define a filter.

\(defcustom file-custom-[NAME]-tail-filter-regexp)
\(defun file-custom-[NAME]-filtered-p)"
  (let* ((str-name name)
         (str-fil str-filter)
         (sn-fil (format "file-custom-%s-tail-filter" str-name))
         (sb-fil (intern sn-fil))
         (sb-reg (intern (format "%s-regexp" sn-fil)))
         (sb-filp (intern (format "%sed-p" sn-fil))))
    `(progn
       (defcustom ,sb-fil ,str-filter
         ,(format "Tail filter for %s." str-name)
         :type 'string
         :group 'config-custom)
       (defvar ,sb-reg
         ,(format "%s\\'" (replace-regexp-in-string "[()|.]" "\\\\\\&" str-fil))
         ,(format "Tail filter regular expressoin for %s." str-name))
       (defun ,sb-filp (fn)
         ,(format "Check whether file FN is %s tail type." str-name)
         (string-match-p ,sb-reg fn)))))

(provide 'file-code)
; file-code.el ends here
