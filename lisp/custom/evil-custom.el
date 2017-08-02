;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

(require 'ui-lib)

(defconst evil-custom-state-string-list
  (list "insert" "normal" "visual" "emacs"
        "motion" "replace" "operator")
  "The list of all the evil state names.")

(defvar evil-custom-initial-state-alist
  (list
   (cons 'messages-buffer-mode 'motion))
  "The alist of (mode . state) for evil to set the initial state.")

(defvar evil-custom-mode-line-alist
  '(("insert" . ("#e80000" . "#ffffff"))
    ("normal" . (nil . nil))
    ("visual" . ("#00e800" . "#ffffff"))
    ("emacs" . ("#444488" . "#ffffff"))
    ("motion" . (nil . nil))
    ("replace" . ("#880000" . "#ffffff"))
    ("operator" . ("#480000" . "#ffffff"))
    )
  )

(eval-when-compile
  (dolist (cus evil-custom-mode-line-alist)
    (unless (member (car cus) evil-custom-state-string-list)
      (error "Evil state not match.(%s)" (car cus)))))

;(eval-when-compile (require 'evil-code))
;(code-defsetter-evil-mode-line-for-state-all)

(provide 'evil-custom)
; evil-custom.el ends here