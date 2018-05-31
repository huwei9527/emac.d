;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

(which-key-mode 1)

;; (which-key-setup-side-window-right)
;; (which-key-setup-side-window-bottom)
;; (which-key-setup-side-window-right-bottom)
;; (which-key-setup-minibuffer)
;; (setq which-key-popup-type 'side-window)
;; (setq which-key-side-window-location 'top)
;; (setq which-key-popup-type 'frame)

;; (setq which-key-idle-delay 1.0)
(code-eval-after-load
 evil
 (setq which-key-show-operator-state-maps t))

(provide 'config-which-key)
;;; config-which-key.el ends here
