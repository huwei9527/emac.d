;;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

;;; Commentary:

;;; Code:

(eval-when-compile (/require-meta format))

(/require-custom format)

(defvar /--format-escape ?\e "Escape character")
(defvar /--format-csi-head (format "%c[" /--format-escape)
  "CSI (Control Sequence Introducer) header.")
(defvar /--format-csi-parameter-seperator ";"
  "CSI (Control Sequence Introducer) parameter seperator.")
(defvar /--format-sgr-command "m"
  "SGR (Select Graphic Redition) command.")

(defun /--format-csi-parameter (&rest args)
  "Make CSI parameter string. \"p1;p2;p3\"."
  (let* (rlt)
    (dolist (para args)
      (push (format "%s" para) rlt)
      (push /--format-csi-parameter-seperator rlt))
    (and args (pop rlt))
    (apply #'concat (nreverse rlt))))

(defun /--format-sgr (&rest args)
  "Make SGR control sequence with parameter ARGS.
\"033[ARGS m\""
  (format "%s%s%s" /--format-csi-head (apply '/--format-csi-parameter args)
	  /--format-sgr-command))

;(/def-format-sgr-variable)
(defvar /--format-sgr-reset 0 "SGR reset command")
(defvar /--format-sgr-color 8 "SGR 8-bits or 16-24 bits color command")
(defvar /--format-sgr-color-256 5 "SGR 8-bits color sub command.")
(defvar /--format-sgr-color-true 2 "SGR 16-24 bits color sub command.")

(defvar /format-sgr-reset (/--format-sgr 0) "SGR reset sequence.")
(defun /format-sgr-reset () "SGR reset sequence." /format-sgr-reset)

(defvar /--format-sgr-font-command-alist
  `((:bold      . 1)
    (:faint     . 2)
    (:italic    . 3)
    (:underline . 4)
    (:blink     . 5)
    (:reverse   . 7)
    (:conceal   . 8)
    (:crossed   . 9))
  "Assoiate list of the SGR font command")

(defvar /--format-sgr-color-command-alist
  `((:black   . 0)
    (:red     . 1)
    (:green   . 2)
    (:yellow  . 3)
    (:blue    . 4)
    (:magenta . 5)
    (:cyan    . 6)
    (:white   . 7)
    (:default . 9))
  "Associate list of the index of SGR color command")

(defun /format-sgr (string &rest args)
  "Format the string according to options in ARGS.
Option:
:reset - reset command
:xxxxx-off and :xxxxx-bg option will first be trimed to :xxxxx and be searched
  in the `/--format-sgr-font-command-list' and
  `/--format-sgr-color-command-list'. Then the code is changed according to the
  tail to be a foreground or background command, or normal command or cancel
  command.
:256-xxx and :256-xxx-bg option will use 38 and 48 command code, the `xxx'
  represent the color code n in `38;5;n'.
:true-xxx-yyy-zzz and :true-xxx-yyy-zzz-bg also use 38 and 48 command code, the
  xxx, yyy, zzz is the r;g;b value of the 24 bits color."
  (let* (cmd opt str off rlt)
    (while args
      (setq opt (pop args))
      (if (eq opt :reset)
	  (push /--format-sgr-reset cmd)
	(setq str (symbol-name opt) off nil)
	(and (string-match "-off\\'" str)
	     (setq opt (intern (substring str 0 (match-beginning 0)))
		   off t))
	(setq rlt (assq opt /--format-sgr-font-command-alist))
	(if rlt
	    (progn (setq rlt (cdr rlt))
		   (and off (setq rlt (+ 20 rlt)))
		   (push rlt cmd))
	  (assert (null off) "Unkonwn option %s" opt)
	  (and (string-match "-bg\\'" str)
	       (setq opt (intern (substring str 0 (match-beginning 0)))
		     off t))
	  (setq rlt (assq opt /--format-sgr-color-command-alist))
	  (if rlt
	      (progn (setq rlt (cdr rlt)
			   rlt (if off (+ 40 rlt) (+ 30 rlt)))
		     (push rlt cmd))
	    (if (string-match "\\`:256-\\(.*\\)" str)
		(progn
		  (if off (push (+ 40 /--format-sgr-color) cmd)
		    (push (+ 30 /--format-sgr-color) cmd))
		  (push /--format-sgr-color-256 cmd)
		  (push (string-to-number (match-string 1 str)) cmd))
	      (if (string-match "\\`:true-\\(.*\\)-\\(.*\\)-\\(.*\\)" str)
		  (progn
		    (if off (push (+ 40 /--format-sgr-color) cmd)
		      (push (+ 30 /--format-sgr-color) cmd))
		    (push /--format-sgr-color-true cmd)
		    (push (string-to-number (match-string 1 str)) cmd)
		    (push (string-to-number (match-string 2 str)) cmd)
		    (push (string-to-number (match-string 3 str)) cmd))
		(error "Unknown option %s" opt)))))))
    (if cmd
	(format "%s%s%s"
		(apply #'/--format-sgr (nreverse cmd))
		string
		/format-sgr-reset)
      string)))

(defun /format-color (fg bg fmt &rest args)
  "Return the format displayed with FG foreground color and BG background
color."
  (or fg (setq fg :default))
  (or bg (setq bg :default-bg))
  (/format-sgr (apply #'format fmt args) fg bg))

(/def-format-color-functions)

(/provide)
;;; lib/format.el ends here
