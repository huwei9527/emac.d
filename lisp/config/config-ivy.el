;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

(eval-when-compile
  (require 'hook-code)
  (require 'keymap-code)
  (require 'ivy-code))

(setq ivy-use-virtual-buffers t
      ivy-count-format "(%d/%d) "
      ivy-wrap t)

(code-defkey-ctl-x
 "C-f" counsel-find-file)

(code-defkey-global
 nil
 "C-s" swiper
 "M-x" counsel-M-x
 ;; "C-S-o" counsel-rhythmbox
 )

(code-defkey-ctl-c
 "g" counsel-git
 "j" counsel-git-grep
 "k" counsel-ag
 "l" counsel-locate
 "C-r" ivy-resume
 )

(code-defkey-ctl-h
 "f" counsel-describe-function
 "v" counsel-describe-variable
 "C-l" counsel-find-library
 "C-i" counsel-info-lookup-symbol
 "u" counsel-unicode-char)

(ivy-mode 1)

(code-define-key
 ivy-minibuffer-map nil
 "C-w" evil-delete-backward-word)

;; Fuzzy search
(setcdr (assq t ivy-re-builders-alist) 'ivy--regex-fuzzy)
(code-remove-fuzzy-search
 (swiper
  counsel-git-grep))
(setq ivy-initial-inputs-alist nil
      ;; No '.'..' directories.
      ivy-extra-directories nil)

(provide 'config-ivy)
;; config-ivy.el ends here
