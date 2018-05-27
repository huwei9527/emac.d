;;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

;;; Commentary:

;;; Code:

(defun /true (&rest _ingore)
  "Ignore any arguments and return t.
This is contrast with `ignore' which always return nil."
  (interactive)
  t)

(defun /false (&rest _ignore)
  "Ignore any arguments and return nil.
This is the opposite of `/true'"
  (interactive)
  nil)

(defun /prepend (list &rest args)
  "Return a list by pushing ARGS in the front of LIST in the coming order.
The last argument is the first element of result list."
  (dolist (e args) (push e list)) list)

(defsubst /regexp-quote (string)
  "Return a regexp string match STRING in following rules.
`  => \\\\`, begin
'  => \\\\', end
\(  => \\\\(, group start
\)  => \\\\), group end
|  => \\\\|, the delimeter in group.
.  => \\\\., dot character
*  => \\\\*, aterisk character"
  (replace-regexp-in-string "[()|.*`']" "\\\\\\&" string))

(defsubst /time-since (last)
  "Seconds between current time and LAST."
  (time-to-seconds (time-since last)))

(defsubst /evenp (n)
  "Return t if N is even."
  (declare (indent defun))
  (eq (% n 2) 0))

(defsubst /oddp (n)
  "Return t if N is odd."
  (declare (indent defun))
  (eq (% n 2) 1))

;;; {{ character prediction
(defun /--character (c)
  "Return a character of C.
If C is a character, return it.
If C is a symbol, return the first character of the symbol-name.
If C is a string, return the first character of the string.
Otherwise nil."
  (if c (cond ((characterp c) c)
	      ((symbolp c) (elt (symbol-name c) 0))
	      ((stringp c) (elt c 0))
	      (t nil))
    nil))

(defun /char-space-p (c)
  "Return t if C is a space character."
  (eq ?\s (char-syntax (/--character c))))

(defun /char-escape-p (c)
  "Return t if C is a escape character."
  (eq ?\\ (char-syntax (/--character c))))

(defun /char-word-p (c)
  "Return t if C is a word constituent."
  (eq ?w (char-syntax (/--character c))))

(defun /char-symbol-p (c)
  "Return t if C is a symbol constituent.
This is the extra character which constitute word besides those the
  type of which is Word."
  (eq ?_ (char-syntax (/--character c))))
;;; }}

(/provide)
;;; lib/core.el ends here
