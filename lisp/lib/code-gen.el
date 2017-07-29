;; -*- lexical-binding : t byte-compile-dynamic : t -*-

(defmacro code-gen-defdir (dir-name doc &optional path)
  "Define a directory.

defcustom config-[DIR-NAME]-directory-name dir-name DOC
defconst config-[DIR-NAME]-directory PATH/DIR-NAME DOC
DIR-NAME / DOC / PATH (a string or nil)"
  ;(unless path (setq path user-emacs-directory))
  (let* ((sym-nm-dir nil)
         (sym-nm-dir-nm nil))
    (unless path (setq path user-emacs-directory))
    (if (null dir-name)
        (setq dir-name "config" sym-nm-dir "config-directory")
      (setq sym-nm-dir (format "config-%s-directory" dir-name)))
    (setq sym-nm-dir-nm (format "%s-name" sym-nm-dir))
    `(progn
       (defcustom ,(intern sym-nm-dir-nm) ,dir-name
         ,(format "The directory name of %s.\n\n%s" dir-name doc)
         :type 'string
         :group 'config-custom)
       (defconst ,(intern sym-nm-dir)
         (file-name-as-directory (expand-file-name ,dir-name ,path))
         ,(format "The directory path of %s.\n\n%s" dir-name doc))
       (when (boundp 'config-directory-list)
         (setq config-directory-list
               (cons ,(intern sym-nm-dir) config-directory-list))))))

(defmacro code-gen-defdir (dir-name doc &optional path)
  "Define a directory.

defcustom config-[DIR-NAME]-directory-name dir-name DOC
defconst config-[DIR-NAME]-directory PATH/DIR-NAME DOC
DIR-NAME / DOC / PATH (a string or nil)"
  ;(unless path (setq path user-emacs-directory))
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
         (setq config-directory-list (cons ,sb-dir config-directory-list))))))

(print (macroexpand '(code-gen-defdir "ABCD" "EFGH" "JKLM")))

(defmacro code-gen-defdir-config (dir-name doc)
  "Define a config directory.

Just a wrapper for custom-defdir (DIR-NAME DOC config-directory)"
  `(code-gen-defdir ,dir-name ,doc ,config-directory))

(defmacro code-gen-defilter-tail (name str-filter)
  "Define a filter.

defcustom file-custom-[NAME]-tail-filter STR-FILTER
defcustom file-custom-[NAME]-tail-filter-regexp
defun file-custom-[NAME]-filtered-p"
  `(progn
     (defcustom ,(intern (format "file-custom-%s-tail-filter" name)) ,str-filter
       ,(format "Tail filter for %s." name)
       :type 'string
       :group 'config-custom)
     (defconst ,(intern (format "file-custom-%s-tail-filter-regexp" name))
       ,(format "%s\\'" (replace-regexp-in-string "[()|.]" "\\\\\\&" str-filter))
       ,(format "Tail filter regular expressoin for %s." name))
     (defsubst ,(intern (format "file-custom-%s-tail-filtered-p" name)) (fn)
       ,(format "Check whether file FN is %s tail type." name)
       (string-match-p
        ,(intern (format "file-custom-%s-tail-filter-regexp" name)) fn))))

(defsubst evil-mode-line-color-custom-name (st pos)
  "The variable symbol name for modeline color of evil state ST.

evil-[ST]-state-mode-line-[POS]."
  (format "evil-%s-state-mode-line-%s" st pos))

(defmacro define-evil-mode-line-color-custom (st pos color)
  "Create custom variable for modeline color of evil state ST.

evil-[ST]-state-mode-line-[POS] = COLOR"
  `(defcustom ,(intern (evil-mode-line-color-custom-name st pos))
     ,color
     ,(format "Modeline %s color for evil %s state." pos st)
     :type 'string
     :group 'config-group))

(defmacro define-evil-mode-line-setter (st &optional fg bg)
  "Create variable and function for setting modeline for evil state ST.

Variable: evil-[ST]-state-mode-line-foreground = FG
Variable: evil-[ST]-state-mode-line-background = BG
Function: set-mode-line-for-evil-[ST]-state ()"
  (let ((fg-str "foreground")
        (bg-str "background"))
    `(progn
       (define-evil-mode-line-color-custom ,st ,fg-str ,fg)
       (define-evil-mode-line-color-custom ,st ,bg-str ,bg)
       (defun ,(intern (format "set-mode-line-for-evil-%s-state" st))
           () ,(format "Modeline color setting function for evil %s state." st)
           (set-mode-line-color
            ,(intern (evil-mode-line-color-custom-name st fg-str))
            ,(intern (evil-mode-line-color-custom-name st bg-str))))
       )))

;; Must be called after
(defmacro define-evil-mode-line-setters ()
  "Create modeline setting variables and functions for all evil state."
  (let* ((ret '(progn)))
    (dolist (esml evil-state-mode-line-alist)
      (setq ret
            (cons
             `(define-evil-mode-line-setter
                ,(car esml) ,(cadr esml) ,(cddr esml))
             ret)))
    (setq ret (nreverse ret))
    (print ret)
    ret)
  )


(provide 'code-gen)
; code-gen.el ends here