;;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

;;; Commentary:

;;; Code:

(eval-when-compile (/require-meta core))

(/def-custom-var gnu/linux-term-set-selection-primary-command
  "xsel -i -p -l /dev/null"
  "Gnu/linux terminal command for `gui-set-selection PRIMARY'.")
(/def-custom-var gnu/linux-term-set-selection-clipboard-command
  "xsel -i -b -l /dev/null"
  "Gnu/linux terminal command for `gui-set-selection CLIPBOARD'.")
(/def-custom-var gnu/linux-term-selection-value-primary-command
  "xsel -o -p -l /dev/null"
  "Gnu/linux terminal command for `gui-selection-value PRIMARY'.")
(/def-custom-var gnu/linux-term-selection-value-clipboard-command
  "xsel -o -b -l /dev/null"
  "Gnu/linux terminal command for `gui-selection-value CLIPBOARD'.")

(/def-custom-var term-set-selection-primary-command
  (cond ((eq system-type 'gnu/linux)
	 /custom-gnu/linux-term-set-selection-primary-command)
	(t nil))
  "Terminal command for `gui-set-selection PRIMARY'.")
(/def-custom-var term-set-selection-clipboard-command
  (cond ((eq system-type 'gnu/linux)
	 /custom-gnu/linux-term-set-selection-clipboard-command)
	(t nil))
  "Terminal command for `gui-set-selection CLIPBOARD'.")
(/def-custom-var term-selection-value-primary-command
  (cond ((eq system-type 'gnu/linux)
	 /custom-gnu/linux-term-selection-value-primary-command)
	(t nil))
  "Terminal command for `gui-selection-value PRIMARY'.")
(/def-custom-var term-selection-value-clipboard-command
  (cond ((eq system-type 'gnu/linux)
	 /custom-gnu/linux-term-selection-value-clipboard-command)
	(t nil))
  "Terminal command for `gui-selection-value CLIPBOARD'.")

(/provide)
;;; custom/select.el ends here
