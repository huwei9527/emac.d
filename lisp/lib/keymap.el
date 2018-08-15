;;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

;;; Commentary:

;;; Code:

(/defun remove-key (keymap keys &optional prefix)
  "Remove `/--keys' in `/--keymap'.
This is done by `assq-delete-all' the key bindings in KEYMAP.
This is different from normal way in which the definition is set to nil.

KEYMAP is `/--keymap' argument.
PREFIX and KEYS are `/--bindgings' arguments."
  (let* ((keys (/--keys keys prefix))
	 (len (1- (length keys))))
    (and (> len 0)
	 (setq keymap
	       (/--keymap (/--lookup-key keymap (seq-take keys len)))))
    (setq keymap (/--keymap keymap))
    (and keymap (assq-delete-all (aref keys len) keymap))))

(/provide)
;;; .el ends here
