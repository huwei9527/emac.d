;;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

;;; Commentary:

;;; Code:

(/require keymap meta lib)

(/message-test-start)

(when nil
  (message "01: %s" (/--kbd "a b c d"))
  (message "02: %s" (/--kbd "abcd"))
  (message "03: %s" (/--kbd 'abcd))
  (message "04: %s" (/--kbd [?a ?b ?c ?d]))
  (message "05: %s" (/--kbd ?a ?b ?c ?d))
  (message "06: %s" (/--kbd (lambda () "ab") "cd"))
  (message "07: %s" (/--kbd ''ab 'cd))
  (message "11: %s" (kbd "a b c d"))
  (message "12: %s" (kbd "abc SPC d"))
  (message "13: %s" (kbd "abc\C-d"))
  (message "14: %s" (kbd "abcC-d"))
  (message "15: %s" (kbd "abcd C-x"))
  (message "16: %s" (kbd "abcd\C-x"))
  (message "21: %s" (/--keys "C-x b" "\C-x a"))
  (message "22: %s" (/--keys "C-x b" [?\C-x ?a]))
  (message "23: %s" (/--keys '\C-x\ b "C-x a"))
  (message "24: %s" (/--keys 'C-x\ a\ \C-x\ b))
  (message "25: %s" (/--keys "C-x a C-x b"))
  (message "26: %s" (/--keys "C-x aC-x b"))
  ;(message "41: %s" (/key-string "C-x l \C-h a"))
  )

(defvar xxx-map nil)
(fset 'xxx-map messages-buffer-mode-map)


(when nil
  (message "%s" (keymapp 'xxx-map))
  (message "%s" (/--keymap messages-buffer-mode-map))
  (message "%s" (/--keymap (symbol-value 'messages-buffer-mode-map)))
  (message "%s" (/--keymap 'xxx-map))
  (message "%s" (/--keymap 'xxx))
  (message "%s" (/--keymap '((%s - mode - map) messages-buffer)))
  )

(when nil
  (message "%s" (/--lookup-key help-map 113))
  (message "%s" (/--lookup-key help-map 52))
  (message "%s" (/--lookup-key help-map [52 105])))

(when nil
  (message "%s" (/--key-definition 'pwd))
  (message "%s" (/--key-definition "C-c C-v"))
  (message "%s" (/--key-definition help-map))
  (message "%s" (/--key-definition (lambda () ignore)))
  )

(when nil
  (message "%s" (/--bindings ?a '("xxx" car
				  "yyy" message
				  "zzz" pwd)))
  (/--sexp-define-key ?a '("xxx" car
			   "yyy" message
			   "zzz" pwd)
		      (dolist (l bindings)
			(message "show: %s" l)))
  )

(when nil
  (let* ((c `("1234" . /show-key-binding))
	 (l (lambda () (interactive) (message "Thank you.")))
	 (k (/--key-sequence "a"))
	 (s "a"))
    (message "%s %s %s %s" (consp c) (consp l) (listp c) (listp l))
    (defmacro /test-command ()
      ""
      `(define-key /test-minor-mode-map
	 ,(kbd "c") ,(/--key-definition s)))
    (print (/--key-definition s))
    (/test-command)
    ))

(when nil
  ; (/ppmacroexpand (/--def-key global-map C-c C-c /show-key-binding))
  ; (/--def-keys global-map C-c C-c /show-key-binding)
  ; (/ppmacroexpand (/--def-key global-map C-c C-c nil))
  ; (/--def-keys global-map C-c C-c nil)
  ; (/--def-keys global-map C-c C-c (lambda () (interactive) (message "lambda")))
  ; (/--def-keys global-map C-c C-v "C-c C-c")
  ; (/--def-keys global-map C-c C-v [?\C-c ?\C-c])
  (message "lookup: %s" (/--lookup-key global-map 'C-c 'C-c))
  )

(when nil
  (message "%s" (/--keymap 'xxx-map))
  (/remove-key 'xxx-map [50] )
  (message "%s" (/--keymap 'xxx-map))
  )

(when nil
  (message "%s" (/--keymap help-map))
  (/remove-key help-map [52 105])
  (message "%s" (/--keymap help-map))
  )

(when nil
  (message "%s" (/--keymap 'xxx-map))
  (/remove-keys 'xxx-map [51] [52] [53] [54] [55])
  (message "%s" (/--keymap 'xxx-map))
  )

(message "%s" 'aaaa)
;(/define-keys global-map "\C-c\C-c" #'/show-key-binding)
(message "after: %s" 'aaa)

(when nil
  (when nil
    (/--define-keys global-map
		    "\C-c\C-c" #'/show-key-binding
		    "\C-c\C-v" #'/show-key-binding))
  (when nil
    (/define-keys global-map C-c
  		  C-v /show-key-binding
  		  C-c /show-key-binding))
  (when nil
    (/define-keys global-map C-c
      C-c /show-key-binding
      C-v /show-key-binding
      C-n /show-key-binding
      ab /show-key-binding))
  (when nil
    (/define-keys global-map
      "C-c C-c" /show-key-binding
      "C-c ab" /show-key-binding))
  (when nil
    (/def-keys global-map)
    (/def-keys global-map C-c)
    ;(/def-keys global-map nil aaa)
    (setq map (/--lookup-key global-map 'C-c))
    (message "map: %s" map)
    (message "map-p: %s" (keymapp map))
    (message "map-sp: %s" (symbolp map))
    (message "map-lp: %s" (listp map))
    (message "map-sp: %s" (symbolp (symbol-function map)))
    (message "map-lp: %s" (listp (symbol-function map)))
    (message "key before: %s" (/--lookup-key map 'C-v))
    (message "map %s" (symbol-function map))
    (/--remove-keys global-map 'C-v 'C-c)
    (/remove-keys global-map C-c\ C-v "C-c C-n")
    (/remove-keys global-map ab)
    (message "key after: %s" (/--lookup-key map 'C-v))
    (message "map %s" (symbol-function map)))
  )

(message "%s" "here")
(when nil
  (/define-keys-mode /test-minor-mode "C-c"
		     "C-v" /show-key-binding
		     "C-c" /show-key-binding
		     ))

(when t
  ;(/ppmacroexpand (/--define-specific-keymap-macro global global-map))
  )

(when nil
  (when t
    (/def-keys-global C-c
		      C-v /show-key-binding
		      C-n /show-key-binding)
    (/def-keys-global C-c))
  ;(/def-keys-ctl-x \\ /show-key-binding)
  (/def-keys-ctl-c C-v /show-key-binding)
  (/def-keys-ctl-h C-i /show-key-binding)
  (/def-keys-ctl-h <tab> /show-key-binding)
  (/def-keys-meta-g a /show-key-binding)
  (/def-keys-ctl-c-prefix global-map C-n /show-key-binding)
  (/def-keys-ctl-c-mode messages-buffer-mode ab /show-key-binding)
  )

(when nil
  (/def-keys-evil-state motion a (lambda () (interactive) (message "fuck")))
  (/def-transient-minor-mode abcd "abcde" `(([?a] . /show-key-binding)) 1234)
  )

(/message-test-end)

(/provide)
;;; test/keymap.el ends here
