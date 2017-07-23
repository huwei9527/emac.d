;; Hide GUI.
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(setq inhibit-startup-screen t
      inhibit-startup-echo-area-message t)

(provide 'config-ui)
; config-ui.el ends here