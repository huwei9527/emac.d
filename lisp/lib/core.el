;;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

;;; Commentary:

;;; Code:

(defun /prepend (list &rest args)
  "Return a list by pushing ARGS in the front of LIST in the coming order.
The last argument is the first element of result list."
  (dolist (e args) (push e list)) list)

(defun /regexp-quote (string)
  "Return a regexp string match STRING in following rules.
`  => \\\\`, begin
'  => \\\\', end
\(  => \\\\(, group start
\)  => \\\\), group end
|  => \\\\|, the delimeter in group.
.  => \\\\., dot character
*  => \\\\*, aterisk character"
  (replace-regexp-in-string "[()|.*`']" "\\\\\\&" string))

(/provide)
;;; lib/core.el ends here
