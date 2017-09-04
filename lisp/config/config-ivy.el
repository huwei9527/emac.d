;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

(eval-when-compile
  (require 'code)
  (require 'ivy-code))

(setq ivy-use-virtual-buffers t
      ivy-count-format "(%d/%d) "
      ivy-wrap t)


(code-defkey-global
 nil
 "C-s" swiper
 "M-x" counsel-M-x
 ;; "C-x C-f" counsel-find-file
 "C-x C-f" find-file-in-project-by-selected
 "C-c g" counsel-git
 "C-c j" counsel-git-grep
 "C-c k" counsel-ag
 "C-c l" counsel-locate
 ;; "C-S-o" counsel-rhythmbox
 "C-c C-r" ivy-resume
 )

(code-define-key
 help-map nil
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