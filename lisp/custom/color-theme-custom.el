;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

(eval-when-compile (require 'file-code))

(code-defdir-config "color-theme"
  "The custom color theme directory for custom color theme. You just
the color theme file (e.g. [theme-name]-theme.el]) in to this directory.
The path and subdirectory of depth 1 will be added automatically.")

(defvar color-theme-custom-solarized-style 'dark
  "Solarized color theme style. (dark or light)")

(provide 'color-theme-custom)
; color-theme-custom.el ends here.