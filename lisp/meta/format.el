;;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

;;; Commentary:

;;; Code:

;; (defvar /--format-sgr-parameter-alist
;;   `((reset         0)
;;     (bold          1)
;;     (faint         2)
;;     (italic        3)
;;     (underline     4)
;;     (slow-blink    5)
;;     ;(rapic-blink   6)
;;     (reverse       7)
;;     (conceal       8)
;;     (crossed-out   9)
;;     (bold-off      21)
;;     (faint-off     22)
;;     (italic-off    23)
;;     (underline-off 24)
;;     (blink-off     25)
;;     (inverse-off   27)
;;     (reveal        28)
;;     (crossed-out-off 29)))

;; (defvar /--format-sgr-color-alist
;;   `((black   0)
;;     (red     1)
;;     (greed   2)
;;     (yellow  3)
;;     (blue    4)
;;     (magenta 6)
;;     (cyan    7)
;;     (white   8)))

;; (defun /--intern-format-sgr-command (name)
;;   "Intern SGR command variable `/--format-sgr-NAME'.
;; See `/--intern'."
;;   (/--intern "--format-sgr-%s" (/--name name)))

;; (defmacro /def-format-sgr-variable ()
;;   ""
;;   (/--sexp-progn
;;     (dolist (attrs /--format-sgr-parameter-alist)
;;       (let* ((name (car attrs))
;; 	     (value (cadr attrs))
;; 	     (doc (car (cddr attrs))))
;; 	(/--sexp-exec
;; 	  `(defvar ,(/--intern-format-sgr-command name) ,value
;; 	     ,(if doc doc (format "SGR %s command" name))))))
;;     (dolist (attrs /--format-sgr-color-alist)
;;       (let* ((name (car attrs))
;; 	     (value (cadr attrs))
;; 	     (doc (car (cddr attrs))))
;; 	(/--sexp-exec
;; 	  `(defvar ,(/--intern-format-sgr-command (format "%s-fg" name))
;; 	     ,(+ value 30)
;; 	     ,(if doc doc (format "SGR %s foreground command" name)))
;; 	  `(defvar ,(/--intern-format-sgr-command (format "%s-bg" name))
;; 	     ,(+ value 40)
;; 	     ,(if doc doc (format "SGR %s background command" name))))))))

(defun /--intern-format-color (name)
  "Intern format color symbol `/format-NAME'.
See `/--intern'"
  (/--intern "format-%s" (/--name name)))

(defmacro /def-format-color-functions ()
  ""
  (declare (indent defun))
  (/--sexp-progn
    (dolist (attrs /--format-sgr-color-command-alist)
      (let* ((name (substring (symbol-name (car attrs)) 1)))
	(/--sexp-exec
	  `(defun ,(/--intern-format-color name) (fmt &rest args)
	     ,(format "Return the format string displayed %s in terminal." name)
	     (apply #'/format-color ,(car attrs) nil fmt args)))))))


(/provide)
;;; meta/format.el ends here
