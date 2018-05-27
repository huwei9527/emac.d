;;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

;;; Commentary:

;;; Code:

(eval-when-compile (/require-meta hook))

(/add-hook (prog-mode-hook)
  rainbow-delimiters-mode)

(/provide)
;;; config/rainbow-delimiters.el ends here
