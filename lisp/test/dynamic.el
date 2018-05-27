;;; Commentary:

;;; Code:

(when t
  (let* ((c `("1234" . /show-key-binding))
	 (l (lambda () (interactive) (message "Thank you.")))
	 (k (/--key-sequence "1234")))
    (message "%s %s %s %s" (consp c) (consp l) (listp c) (listp l))
    (defmacro /test-command ()
      ""
      `(define-key /test-minor-mode-map
	 ,(kbd "c") ,(/--key-definition l)))
    (print (/--key-definition l))
    (/test-command)
    ))
(/provide)
;;; test/dynamic.el ends here
