;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

(eval-when-compile
  (require 'hook-code)
  (require 'keymap-code))

(code-eval-after-load
 python
 (elpy-enable))

(code-eval-after-load
 elpy
 (setq elpy-rpc-python-command "python3"))

(provide 'config-python)
;;; config-python.el ends here
