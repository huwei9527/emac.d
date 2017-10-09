;; -*- lexical-binding : t byte-compile-dynamic : t -*-

;;; {{
(defun char-space-p (c)
  "Return t if C is a space character."
  (eq ?\s (char-syntax c)))

(defun char-word-p (c)
  "Return t if C is a word constituent."
  (eq ?w (char-syntax c)))
;;; }}

(defun ignore-true (&rest _ignore)
  "This function accepts any number of arguments but ignore them 
and return t"
  t)

(defmacro swap (var1 var2)
  "Swap two value of two variable."
  (let* ((tmp (make-symbol "usym-tmp")))
    `(setq ,tmp ,var1 ,var1 ,var2 ,var2 ,tmp)))

;;; {{
(defun sequence-length (se)
  "Return the length of the sequence"
  (if (listp se) (safe-length se) (length se)))

(defun pure-consp (obj)
  "Return non-nil if OBJ is a con-cell the cdr of which is 
neither a cons-cell nor nil."
  (and (consp obj) (cdr obj) (not (consp (cdr obj)))))

(defun to-list (el)
  "Convert EL to a list"
  (cond
   ((listp el) el)
   ((arrayp el) (append el nil))
   (t (list el))))

(defun remove-nil (seq)
  "Remove nil from sequence SEQ and return a *list*"
  (to-list (remove nil seq)))

(defmacro sequence--push-stack (el stack)
  "Push EL in STACK if el is a sequence"
  `(when (and el (sequencep el)) (push el stack)))

(defun sequence-filter (seq rep)
  "Parse over all the element of the sequence SEQ recursively and
apply REP to it. If REP return a cons-cell, replace the element."
  (let* ((curr seq) stack rlt el len)
    (while curr
      (cond
       ((listp curr)
	(while curr
	  (setq el (car curr)
		rlt (funcall rep el))
	  (cond
	   ((null rlt) (sequence--push-stack el stack))
	   ((consp rlt) (setcar curr (car rlt))))
	  (setq el (cdr curr))
	  (when (and el (not (consp el)))
	    (setq rlt (funcall rep el))
	    (cond
	     ((null rlt) (sequence--push-stack el stack))
	     ((consp rlt) (setcdr curr (car rlt))))
	    (setq el nil))
	  (setq curr el)))
       ((arrayp curr)
	(setq len (length curr))
	(dotimes (i len)
	  (setq el (aref curr i)
		rlt (funcall rep el))
	  (cond
	   ((null rlt) (sequence--push-stack el stack))
	   ((consp rlt) (setf (aref curr i) (car rlt)))))))
      (setq curr (pop stack)))))
;;; }}

(provide 'core-lib)
;;; core-lib.el ends here
