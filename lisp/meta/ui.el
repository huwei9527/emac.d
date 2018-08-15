;;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

;;; Commentary:

;;; Code:

(eval-when-compile (/require-meta core))

(/defvar* fgname "foreground" "Foreground face subname")
(/defvar* bgname "background" "Background face subname")

(/defun* intern-face (&optional fg bg)
  (declare (indent defun))
  (:documentation (format "Intern face name string.
If fg and bg are both non-nil, use `FG-BG'.
If fg is nil and bg is non-nil, use `BG-%s'.
If fg is non-nil and bg is nil, use `FG-%s'.
If fg and bg are both nil, user `nil'." /--fgname /--bgname))
  (if fg (if bg (/--intern "%s-%s" (/--name fg) (/--name bg))
	   (/--intern "%s-%s" (/--name fg) /--fgname))
    (and bg (/--intern "%s-%s" (/--name bg) /--bgname))))

(/--defformat foreground :format "foreground")
(/--defformat background :format "background")

(/defvar* color-alist
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

(/defmacro* defface-single (attrs &optional fg)
  "Define face containing one single color.
ATTRS is a list of form (name tty hex):
 name (symbol) - color name
 tty  (string) - tty display color
 hex  (string) - fall through color
If FG is nil, the face is a background face in `background /--format'.
Otherwise the face is a foreground face in `foreground /--format'.
The color of the face is set according to ATTRS."
  (declare (indent defun))
  (let* ((attrs (/--list attrs))
         (prop (if fg :foreground :background))
	 name tty hex)
    (cl-multiple-value-setq (name tty hex) attrs)
    (and (boundp '/--face-list)
	 (push (/--intern (if fg 'foreground 'background) name) /--face-list))
    `(/defface ((%s - ,(if fg 'foreground 'background)) ,name)
       '((default ,prop ,(symbol-name name))
	 (((class color) (type tty) (min-colors 256)) ,prop ,tty)
         (t ,prop ,hex))
       ,(format "Single %s face.\ntty: %s\nt  : %s" name tty hex))))


(/defmacro* defface-double (fattrs battrs)
  "Define double faces."
  (let* ((fattrs (/--list fattrs)) (battrs (/--list battrs))
	 (fname (car fattrs)) (bname (car battrs)))
    (and (boundp '/--face-list)
	 (push (/--intern "%s-%s" fname bname) /--face-list))
    `(/defface ("%s-%s" ,fname ,bname)
	       '((t :inherit (,(/--intern '(%s - foreground) fname)
			      ,(/--intern '(%s - background) bname))))
       ,(format "Double %s and %s face.\ntty: (%s %s)\nt  : (%s %s)"
		fname bname
		(nth 1 fattrs) (nth 1 battrs) (nth 2 fattrs) (nth 2 battrs)))))

(/defmacro* defface-simple ()
  "Define simple faces."
  (let* (white black colors)
    (/--sexp-progn
      ;; single color face
      (dolist (attrs /--color-alist)
	(/--sexp-exec
	  `(/--defface-single ,attrs fg) `(/--defface-single ,attrs)))
      (setq colors /--color-alist
	    white (car colors) colors (cdr colors)
	    black (car colors) colors (cdr colors))
      ;; black and white face
      (/--sexp-exec
	`(/--defface-double ,white ,black) `(/--defface-double ,black ,white))
      ;; black color face
      (dolist (battrs `(,white ,black))
	(dolist (cattrs colors)
	  (/--sexp-exec
	    `(/--defface-double ,battrs ,cattrs)
	    `(/--defface-double ,cattrs ,battrs)))))))


(/provide)
;;; meta/ui.el ends here
