;;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

;;; Commentary:

;;; Code:

(eval-when-compile (/require-meta core))

(defvar /fgname "foreground")
(defvar /bgname "background")

(defvar /color-alist
  ;;            256          t
  '((white    "color-255" "#ffffff")
    (black    "color-16"  "#000000")
    (red      "color-196" "#ff0000")
    (green    "color-46"  "#00ff00")
    (blue     "color-21"  "#0000ff")
    (yellow   "color-226" "#ffff00")
    (magenta  "color-201" "#ff00ff")
    (cyan     "color-51"  "#00ffff")
    )
  "Face color alist.")

(defvar /face-list nil "")
(defvar /attrs (car /color-alist))

(defmacro /defface--single (attrs f/b)
  (declare (indent defun))
  (:documentation
   (format "Define face containing one single color.
ATTRS is a list of form (name tty hex):
 name (symbol) - face name `name-F/B'
 tty  (string) - tty display color
 hex  (string) - fall through color
F/B is one of \"%s\" for \"%s\"" /fgname /bgname))
  (let* ((attrs (symbol-value attrs))
	 (cr (nth 0 attrs))
         (name (symbol-name cr))
         (tty (nth 1 attrs))
         (hex (nth 2 attrs))
         (sym-fb (/intern ":%s" f/b))
         (sym-face (/intern "%s-%s" name f/b)))
    (push sym-face /face-list)
    `(defface ,sym-face
       '((((class color) (type tty) (min-colors 256)) ,sym-fb ,tty)
         (t ,sym-fb ,hex))
       ,(format "%s-%s face. tty : %s; t : %s" name f/b tty hex))))

(pp (macroexpand '(/defface--single /attrs /fgname)))

(defmacro code-defface-foreground (cr-list)
  ""
  `(code-defface-foreground-or-background-raw ,cr-list "foreground"))

(defmacro code-defface-background (cr-list)
  ""
  `(code-defface-foreground-or-background-raw ,cr-list "background"))

(defmacro code-defface-foreground-and-background-raw (fg-list bg-list)
  (let* ((fg (car fg-list))
         (bg (car bg-list))
         (fg-name (symbol-name fg))
         (bg-name (symbol-name bg))
         (sym-face (intern-format "%s-%s" fg-name bg-name)))
    (push sym-face code-basic-color-face-list)
    `(defface ,sym-face
       '((t :inherit (,(intern-format "%s-foreground" fg-name)
                      ,(intern-format "%s-background" bg-name))))
       ,(format "%s-%s face." (capitalize fg-name) (capitalize bg-name)))))

(defmacro code-defface-basic-color ()
  "Define face with basic colors."
  (let* ((wb-alist (list (car code-color-alist)
                         (cadr code-color-alist)))
         (cr-alist (nthcdr 2 code-color-alist)))
    (code-progn
     (dolist (list-cr code-color-alist)
       (code-item `(code-defface-foreground ,list-cr)
                  `(code-defface-background ,list-cr)))
     (code-item `(code-defface-foreground-and-background-raw
                  ,(car wb-alist) ,(cadr wb-alist))
                `(code-defface-foreground-and-background-raw
                  ,(cadr wb-alist) ,(car wb-alist)))
     (dolist (list-wb wb-alist)
       (dolist (list-cr cr-alist)
         (code-item `(code-defface-foreground-and-background-raw
                      ,list-cr ,list-wb)
                    `(code-defface-foreground-and-background-raw
                      ,list-wb ,list-cr)))))))


(/provide)
;;; meta/ui.el ends here
