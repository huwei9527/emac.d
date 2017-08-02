;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

(require 'evil-custom)
(eval-when-compile (require 'evil-code))


(code-defsetter-evil-mode-line-state-all)
(code-defsetter-evil-mode-line)

(setq scnt 0)
(advice-add 'switch-to-buffer :after (lambda (&rest args) "" (message "Switch %d" (setq scnt (1+ scnt)))))

(provide 'evil-lib)
; evil-lib.el ends here