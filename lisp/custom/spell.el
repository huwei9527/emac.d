;;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

;;; Commentary:

;;; Code:

(eval-when-compile (/require-meta file core))

(/def-config-directory spell
  "The directory to store `ispell' personal dictionary.")

(/def-custom-var spell-personal-file-extension ".personal"
  "Spell check personal dictionary filename extension.")

(/def-custom-var spell-personal-default-file "personal"
  "Default personal dictionary filename.")

(/def-custom-var spell-personal-regexp-alist
  `(("test.txt\\'" "test")
    ("est.txt" "testnew"))
  "Specific spell check personal dictionary for each filename regexp.
Each entry is a list. 
The first element is the regexp. 
The second element is the filename for that major mode. If the second
  element is ommited, the symbol name of the major mode is
  used. Besides the filename, a filename extension
  `/custom-spell-personal-file-extension' is added automatically.")

(/def-custom-var spell-personal-mode-alist
  '((c-mode)
    (java-mode)
    (python-mode))
  "Specific spell check personal dictionary file for major modes.
Each entry is a list. 
The first element is the major mode symbol. 
The second element is the filename for that major mode. If the second
  element is ommited, the symbol name of the major mode is
  used. Besides the filename, a filename extension
  `/custom-spell-personal-file-extension' is added automatically.")

(/provide)
;;; custom/spell.el ends here
