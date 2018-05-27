;;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

;;; Commentary:

;;; Code:

(/require-meta keymap)

(/message-test-start)
(when nil
  (message "11: %s" (kbd "a b c d"))
  (message "12: %s" (kbd "abc SPC d"))
  (message "13: %s" (kbd "abc\C-d"))
  (message "14: %s" (kbd "abcC-d"))
  (message "15: %s" (kbd "abcd C-x"))
  (message "16: %s" (kbd "abcd\C-x"))
  (message "21: %s" (/--key-sequence "C-x a" "\C-x l"))
  (message "22: %s" (/--key-sequence "C-x a" [?\C-x ?l]))
  (message "23: %s" (/--key-sequence [?\C-x ?a] "C-x l"))
  (message "24: %s" (/--key-sequence [?\C-x ?a] [?\C-x ?l]))
  (message "25: %s" (/--key-sequence "C-x l \C-x a"))
  (message "26: %s" (/--key-sequence [?\C-x ?l ?\C-x ?a]))
  (message "31: %s" (/--key-sequence "C-a bcd" "efgh"))
  (message "32: %s" (/--key-sequence "\C-abcd" "efgh"))
  (message "33: %s" (/--key-sequence 'abcd 'efgh))
  (message "34: %s" (/--key-sequence 'C-a\ bcd 'efgh))
  (message "35: %s" (/--key-sequence '\C-a\ bcd 'efgh))
  (message "41: %s" (/key-string "C-x l \C-h a")))

(when nil
  (setq abc\ d 1234)
  (message "%s %s %s" abc\ d 'abc\ d '\\C-abcd)
  (message "%s" "\C-abcd")
  (message "%s" '\C-a\0bcd)
  (message "%s" '\?abcdefg)
  (message "%s" (equal "\C-abcd" (symbol-name '\C-abcd)))

  (let* ((a 'b))
    (message "%s" a)
    (setq a `(quote ,a))
    (message "%s" a)))

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
  (when nil
    (/--def-keys global-map
		  "\C-c\C-c" #'/show-key-binding
		  "\C-c\C-v" #'/show-key-binding))
  (when nil
    (/def-keys global-map C-c
		C-v /show-key-binding
		C-c /show-key-binding))
  (when nil
    (/def-keys global-map C-c
      C-c /show-key-binding
      C-v /show-key-binding
      C-n /show-key-binding
      ab /show-key-binding))
  (when nil
    (/def-keys global-map
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
(/message-test-end)

(/provide)
;;; test/keymap.el ends here
