;;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

;;; Commentary:

;;; Code:

(eval-when-compile (/require-meta keymap hook ivy))

(require 'evil)
(require 'ivy)

(setq ivy-use-virtual-buffers t
      ivy-count-format "(%d/%d) "
      ivy-wrap t
      ivy-initial-inputs-alist nil ; no ^ in minibuffer
      ivy-extra-directories nil    ; no '.' '..' directories.
      )

(/def-keys-ctl-x
 C-f counsel-find-file
 )

(/def-keys-global
 C-s swiper
 C-r ivy-resume
 M-x counsel-M-x
 )

(/def-keys-ctl-c
 g counsel-git
 j counsel-git-grep
 k counsel-ag
 l counsel-locate)

(/def-keys-ctl-h
 f counsel-describe-function
 v counsel-describe-variable
 o counsel-describe-face
 C-l counsel-find-library
 C-i counsel-info-lookup-symbol
 u counsel-unicode-char
 )

(/def-keys ivy-minibuffer-map
  C-w evil-delete-backward-word)

;; setup fuzzy search
(setcdr (assq t ivy-re-builders-alist) 'ivy--regex-fuzzy)
;; setup regex-plus search for specific command.
(/set-regex-plus-search swiper counsel-git-grep)

(ivy-mode)

(/provide)
;;; config/ivy.el ends here
