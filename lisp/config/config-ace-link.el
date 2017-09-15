;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

(eval-when-compile
  (require 'code)
  (require 'keymap-code))

(code-eval-after-load
 info
 (code-define-key
   Info-mode-map nil
   "o" ace-link-info))

(code-eval-after-load
 help-mode
 (code-define-key
   help-mode-map nil
   "o" ace-link-help))

(code-eval-after-load
 cus-edit
 (message "custom load")
 (code-define-key
   custom-mode-map nil
   "o" ace-link-custom
   )
 )


(provide 'config-ace-link)
;;; config-ace-link.el ends here
