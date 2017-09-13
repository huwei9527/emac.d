;; -*- lexical-binding : t byte-compile-dynamic : t -*-

(defun sequence-element-filter (seq op)
  "Parse the elements of the sequence SEQ recursively and apply 
funtion OP to each element.

OP must be a function take two argument: (OP el pos path) where
el - the current element during the parsing
pos - the index of 'el' in its parent element
path - the ancestor elements that contains 'el' in the reverse order
Note that (car path) is the parent elment of 'el'.  If OP return non-nil, 
'el' will be parsed recursively. Otherwise 'el' will be dropped."
  (when (and seq (functionp op))
    (let* ((stack (list (cons seq nil)))
	   stack-curr seq-curr seq-par seq-pos path)
      (while stack
	(setq stack-curr (pop stack)
	      seq-curr (car stack-curr)
	      seq-par (cadr stack-curr)
	      seq-pos (cddr stack-curr))
	(while (not (eq seq-par (car path))) (pop path))
	(when (and (funcall op seq-curr seq-pos path) (sequencep seq-curr))
	  (let* ((len (length seq-curr)))
	    (dotimes (i len)
	      (push (cons (elt seq-curr i)
			  (cons seq-curr i)) stack))))
	(push seq-curr path)))))

(defun sequence-equal (seq1 seq2)
  "Return t if all the corresponding elements of SEQ1 and SEQ2 are equal.
Ignore the sequence type."
  (if (eq seq1 seq2) t
    (let* ((seq1-p (sequencep seq1))
	   (seq2-p (sequencep seq2)))
      (cond
       ((and seq1-p seq2-p)
	(let* ((len (length seq1)) rlt)
	  (when (eq len (length seq2))
	    (setq rlt t)
	    (catch 'tag-break
	      (dotimes (i len)
		(unless (sequence-equal (elt seq1 i) (elt seq2 i))
		  (setq rlt nil)
		  (throw 'tag-break nil)))))
	  rlt))
       ((not (or seq1-p seq2-p))
	(equal seq1 seq2))
       (t nil)))))

(defun collect-non-nil-element (seq)
  "Return the list of the non-nil element in sequence SEQ."
  (when (and seq (sequencep seq))
    (let* ((len (length seq))
	   rlt el-curr)
      (dotimes (i len)
	(when (setq el-curr (elt seq i))
	  (push el-curr rlt)))
      (nreverse rlt))))

(provide 'sequence-lib)
;;; sequence-lib.el ends here
