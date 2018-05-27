;;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

;;; Commentary:

;;; Code:

(eval-when-compile (/require-meta core))

(which-key-mode)

(/eval-after-load evil (setq which-key-show-operator-state-maps t))

(/provide)
;;; config/which-key.el ends here
