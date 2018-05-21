;;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

;;; Commentary:

;;; Code:

(/require-custom theme)
(/require-lib file)

;; theme path
(eval-after-load 'custom
  (progn
    (setq custom-theme-directory /custom-config-theme-directory)
    (/add-subdirectory-to-list /custom-config-theme-directory
			       'custom-theme-load-path 1)))

(load-theme 'tango-dark 'no-confirm)

(/provide)
;;; config/theme.el ends here
