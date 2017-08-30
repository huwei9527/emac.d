;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

(require 'path-lib)
(require 'color-theme-custom)
(eval-when-compile
  (require 'ui-code))

;; Custom color theme loas path.
(add-directory-to-list config-color-theme-directory 'custom-theme-load-path 1)

;; 256-color terminal surpport
(setq solarized-termcolors 256)

;; Using solarized color theme.
(if (daemonp)
    ;; Make emacs client using solarized dark theme
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (with-selected-frame frame
                  (code-load-solarized-theme))))
  (code-load-solarized-theme))

(provide 'config-theme)
; config-theme.el ends here.