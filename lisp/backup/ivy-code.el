;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

(eval-when-compile
  (require 'code))

(defmacro code-remove-fuzzy-search (fun-list)
  "Set fuzzy search for functions in fun-list"
  (let* ((sym-list (code-get-list fun-list)))
    (code-progn
     (dolist (fn sym-list)
       (code-item
         `(add-to-list 'ivy-re-builders-alist
                       '(,fn . ivy--regex-plus)))))))


(provide 'ivy-code)
;; ivy-code.el ends here