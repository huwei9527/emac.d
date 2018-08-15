;;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

(/require keymap meta lib)

(message "xxxxxx: 1")
;(pp (macroexpand-all '(/define-keys global-map "\C-c\C-c" #'/show-key-binding)))
(message "xxxxxx: 2")

(/provide)
