;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-


;; Must before any configuration package.
;(require 'global-custom (expand-file-name "lisp/custom/global-custom.el" user-emacs-directory))

(setq emacs-load-start-time (current-time))

(load-file (expand-file-name "test_compile.elc" user-emacs-directory))
(message "AFDAFAF")
(if (fboundp 'bc_testa) (message "Exist."))
;(disassemble 'bc_testa)
;(disassemble 'bc_testb)
(disassemble 'bc_testc)
(disassemble 'bc_testd)
(disassemble 'bc_test_macroa)
;(bc_testa)
;(bc_testb)
;(bc_testc)


(defvar emacs-load-time (time-to-seconds (time-since emacs-load-start-time)))
(message "%f" emacs-load-time)
(provide 'test-compile)
; test-compile.el ends here