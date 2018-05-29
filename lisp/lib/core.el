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

(defun /call-silently (fun &rest args)
  "Call FUN with ARGS with no message."
  (let* ((inhibit-message t))
    (apply fun args)))

(defsubst /neq (obj1 obj2)
  "Return t if OBJ1 is not `eq' OBJ2."
  (not (eq obj1 obj2)))

(defsubst /nequal (obj1 obj2)
  "Return t if OBJ1 is not `equal' OBJ2."
  (not (equal obj1 obj2)))

(defun /prepend (list &rest args)
  "Return a list by pushing ARGS in the front of LIST in the coming order.
The last argument is the first element of result list."
  (dolist (e args) (push e list)) list)

(defsubst /--regexp-quote (string)
  "Return the transformed STRING.\nSee `/regexp-quote'."
  (replace-regexp-in-string "[()|.*`']" "\\\\\\&" (/--name string)))

(defun /regexp-quote (&rest args)
  "Return a regexp string match STRING in following rules.
`  => \\\\`, begin
'  => \\\\', end
\(  => \\\\(, group start
\)  => \\\\), group end
|  => \\\\|, the delimeter in group
.  => \\\\., dot character
*  => \\\\*, aterisk character

If ARGS contains one arguments and is a string, return the transformed
string.
If ARGS contains one arguments and is a list, return the list with
each element tranformed.
If ARGS contains more than one arguments, then none of them should be
list, and return the list with each element in ARGS transformed."
  (let* (rlt)
    (if (eq 1 (length args))
	(if (stringp (car args))
	    (setq rlt (/--regexp-quote (car args)))
	  (setq args (car args))))
    (or rlt (dolist (str args) (push (/--regexp-quote str) rlt)))
    (if (listp rlt) (nreverse rlt) rlt)))

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

(defun /char-at-point-word-p ()
  "Return non-nil if the character just before the point is a word.
See `preceding-char', `/char-word-p', `/char-symbol-p'."
  (and (not (eq (point) (line-beginning-position)))
       (let* ((c (preceding-char)))
	 (or (/char-word-p c)
	     (/char-symbol-p c)))))
;;; }}

;;; {{ shell-command
;; FIXME: use emacs asynchronous process to feed input.
(defun /shell-command-to-string (cmd &optional input)
  "Execute CMD.
If INPUT is non-nil, it is feed to the standard input."
  (if input
      (shell-command-to-string
       (format "echo %s | %s" (shell-quote-argument input) cmd))
    (shell-command-to-string cmd)))
;;; }}

(defun /wait-for-events (&optional sec n)
  "Wait for N (default 1) events for at most SEC milliseconds.
If SEC is nil, wait infinitely."
  (or n (setq n 1))
  (let* ((time (if sec (time-add sec nil) nil)) e events)
    (catch 'break-tag
      (dotimes (i n)
	;(push (read-event nil nil sec) events)
	(and (setq e (read-event nil nil sec)) (push e events))
	;(message "wait: %s %s" events (key-description events))
	(and time (time-less-p time nil) (throw 'break-tag nil))
	(setq sec (float-time (time-subtract time nil)))))
    (vconcat (nreverse events))
    ))

(defun /put-back-events (&optional events)
  "Put EVENTS back to `unread-command-events'."
  (or events (setq events (this-command-keys-vector)))
  (setq unread-command-events (append unread-command-events events nil)))

(/provide)
;;; lib/core.el ends here
