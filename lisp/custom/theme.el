;;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

;;; Commentary:

;;; Code:

(eval-when-compile (/require-meta file))

(/def-config-directory theme
  "The custom color theme directory for custom color theme.
Put the color theme file (e.g. [theme-name]-theme.el]) in to this
  directory. The path and subdirectory of depth 1 will be added
  automatically.")

(/provide)
;;; custom/theme.el ends here
