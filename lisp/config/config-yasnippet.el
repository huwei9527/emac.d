;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

(eval-when-compile
  (require 'hook-code))

(require 'yasnippet-custom)

(code-eval-after-load
 yasnippet
 (setq yas-snippet-dirs
       (remove yas--default-user-snippets-dir yas-snippet-dirs))
 (setq yas--default-user-snippets-dir config-snippets-directory)
 (add-to-list 'yas-snippet-dirs config-snippets-directory))

(provide 'config-yasnippet)
;;; config-yasnippet.el ends here
