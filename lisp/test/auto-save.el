;;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

;;; Commentary:

;;; Code:

(/require-lib auto-save)

(/message-test-start)
(setq /custom-auto-save-interval 2)
; (/idle-save-mode)
; (/focus-save-mode)
(/message-test-end)

(/provide)
;;; test/auto-save.el ends here
