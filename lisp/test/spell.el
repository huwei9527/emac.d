;;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

;;; Commentary:

;;; Code:

(/require-custom spell)

(/message-test-start)
;; (/add-hook (flyspell-mode-hook)
;;   (lambda () ""
;;     (message "%s" (buffer-name))
;;    ))
(/message-test-end)

(/provide)
;;; test/spell.el ends here
