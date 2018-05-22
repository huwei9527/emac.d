;;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

;;; Commentary:

;;; Code:

(eval-when-compile (/require-meta core))

(defvar /--fgname "foreground" "Foreground face subname")
(defvar /--bgname "background" "Background face subname")

(defun /--intern-face (&optional fg bg)
  (declare (indent defun))
  (:documentation (format "Intern face name string.
If fg and bg are both non-nil, use `FG-BG'.
If fg is nil and bg is non-nil, use `BG-%s'.
If fg is non-nil and bg is nil, use `FG-%s'.
If fg and bg are both nil, user `nil'." /--fgname /--bgname))
  (if fg (if bg (/--intern "%s-%s" (/--name fg) (/--name bg))
	   (/--intern "%s-%s" (/--name fg) /--fgname))
    (and bg (/--intern "%s-%s" (/--name bg) /--bgname))))

(defvar /--color-alist
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

(defmacro /defface--single (attrs &optional fg)
  (declare (indent defun))
  (:documentation
   (format "Define face containing one single color.
ATTRS is a list of form (name tty hex):
 name (symbol) - face name `name-F/B'
 tty  (string) - tty display color
 hex  (string) - fall through color
If FG is non-nil, the single color is set to foreground and F/B is
  %s, otherwise the single color is set to background and F/B is %s."
	   /--fgname /--bgname))
  (let* ((attrs (/--list attrs)) (tty (nth 1 attrs)) (hex (nth 2 attrs))
         prop face)
    (if fg (setq prop :foreground face (/--intern-face (nth 0 attrs) nil))
      (setq prop :background face (/--intern-face nil (nth 0 attrs))))
    (and (boundp '/--face-list) (push face /--face-list))
    `(defface ,face
       '((((class color) (type tty) (min-colors 256)) ,prop ,tty)
         (t ,prop ,hex))
       ,(format "Single face %s.\ntty : %s; t : %s" face tty hex))))

(defmacro /defface--double (fattrs battrs)
  (let* ((fattrs (/--list fattrs)) (battrs (/--list battrs))
	 (fname (car fattrs)) (bname (car battrs))
         (face (/--intern-face fname bname)))
    (and (boundp '/--face-list) (push face /--face-list))
    `(defface ,face
       '((t :inherit (,(/--intern-face fname nil) ,(/--intern-face nil bname))))
       ,(format "Double face %s.\ntty : (%s %s); t : (%s %s)" face
		(nth 1 fattrs) (nth 1 battrs) (nth 2 fattrs) (nth 2 battrs)))))

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
