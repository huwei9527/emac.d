;; -*- lexical-binding : t byte-compile-dynamic : t -*-

(defmacro code-defdir (dir-name doc &optional path)
  "Define a directory.

\(defcustom config-[DIR-NAME]-directory-name dir-name DOC)
\(defconst config-[DIR-NAME]-directory PATH/DIR-NAME DOC)
DIR-NAME / DOC / PATH (a string or nil)"
  (let* ((str-name (or dir-name "config"))
         (str-path (or path user-emacs-directory))
         (str-doc doc)
         (sn-dir (if dir-name (format "config-%s-directory" dir-name)
                   "config-directory"))
         (sb-dir (intern sn-dir))
         (sb-name (intern (format "%s-name" sn-dir))))
    `(progn
       (defcustom ,sb-name ,str-name
         ,(format "The directory name of %s.\n\n%s" str-name str-doc)
         :type 'string
         :group 'config-custom)
       (defconst ,sb-dir
         ,(file-name-as-directory (expand-file-name str-name str-path))
         ,(format "The directory path of %s.\n\n%s" str-name str-doc))
       (and (boundp 'config-directory-list)
         (setq config-directory-list (cons ',sb-dir config-directory-list))))))

(defmacro code-defdir-config (dir-name doc)
  "Define a config directory.

Just a wrapper for (code-defdir DIR-NAME DOC config-directory)"
  (let* ((str-name dir-name)
         (str-doc doc)
         (str-cd config-directory))
  `(code-defdir ,str-name ,str-doc ,str-cd)))

(defmacro code-defilter-tail (name str-filter)
  "Define a filter.

\(defcustom file-custom-[NAME]-tail-filter STR-FILTER)
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
       (defconst ,sb-reg
         ,(format "%s\\'" (replace-regexp-in-string "[()|.]" "\\\\\\&" str-fil))
         ,(format "Tail filter regular expressoin for %s." str-name))
       (defsubst ,sb-filp (fn)
         ,(format "Check whether file FN is %s tail type." str-name)
         (string-match-p ,sb-reg fn)))))

(provide 'file-code)
; file-code.el ends here