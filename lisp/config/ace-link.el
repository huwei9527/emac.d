;;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

;;; Commentary:

;;; Code:

(eval-when-compile (/require-meta core keymap))

;; Info-mode
(/def-keys-mode Info-mode o ace-link-info)

;; help-mode
(/def-keys-mode help-mode o ace-link-help)

;; custom-mode
(/eval-after-load cus-edit
  (/def-keys-mode custom-mode o ace-link-custom))

(/provide)
;;; config/ace-link.el ends here
