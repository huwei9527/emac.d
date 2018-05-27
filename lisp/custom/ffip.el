;;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

;;; Commentary:

;;; Code:

(eval-when-compile (/require-meta core))

(/def-custom-var ffip-default-directory "~/Projects/mab/"
  "Default directory for ffip if ffip can't determine the project directory.")

(/def-custom-var ffip-large-directory-list
  '("~"
    "~/Projects"
    "~/Codes"))

(/provide)
;;; custom/ffip.el ends here
