;;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

;;; Commentary:

;;; Code:

(eval-when-compile (/require-meta core keymap))

(/def-keys-ctl-c
 C-SPC ace-jump-mode-pop-mark)

(/def-keys-meta-g
 M-l goto-line     ; move goto-line binding to M-g M-l
 M-g ace-jump-mode ; use orignal got-line binding
 )

(/eval-after-load ace-jump-mode
  (ace-jump-mode-enable-mark-sync))


(/provide)
;;; config/ace-jump-mode.el ends here
