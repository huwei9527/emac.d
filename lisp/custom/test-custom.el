;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

(defvar test-debug-flag t
  "Debug flag.")

;;; Debug flag.
;; (setq debug-on-error t)
;; (setq edebug-all-defs t)
;; (setq edebug-all-forms t)
;; (switch-to-buffer "*ShowFace*")
(save-selected-window (switch-to-buffer-other-window "*Messages*"))
;; (find-file "test.txt")
;; (split-window-right)


(provide 'test-custom)
;; test-custom.el ends here
