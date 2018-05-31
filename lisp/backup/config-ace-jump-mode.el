;;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

(eval-when-compile
  (require 'code)
  (require 'keymap-code)
  (require 'hook-code)
  (require 'miscellany-code))

(code-define-temporary-minor-mode
 ace-jump-pop-mark
 "Pop ace jump mark"
 `((,(kbd "SPC") . ace-jump-mode-pop-mark)))

(code-defkey-ctl-c
 "SPC" ace-jump-mode
 "C-SPC" ace-jump-pop-mark-mode)

(code-eval-after-load
 ace-jump-mode
 (ace-jump-mode-enable-mark-sync))


(provide 'config-ace-jump-mode)
;;; config-ace-jump-mode.el ends here
