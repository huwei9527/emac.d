;;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

;;; Commentary:

;;; Code:

(eval-when-compile (/require-meta core))

;; Ignore error
(/def-custom-var ignore-error-predicate-list
  `(/error-read-only-p
    /error-buffer-boundary-p
    /error-history-p
    )
  "List of the predicate functions for errors to be ignore.
The function take three argument (DATA, CONTEXT, CALLER).
If one of the functions return non-nil, the error is ingored.")

(/def-custom-var buffer-major-mode-ignore-list
  `(dired-mode
    help-mode
    Info-mode
    inferior-python-mode)
  "List of major-modes of which the buffer will be ignored in `next-buffer'.")

(/def-custom-var temporary-buffer-major-mode-list
  `(dired-mode
    help-mode
    special-mode
    Custom-mode
    process-menu-mode
    Buffer-menu-mode)
  "The list of major mode of which the buffer is a temporary buffer.")

(/def-custom-var close-other-buffer-function nil
  "The function called when `close-other-buffer'.")
(make-variable-buffer-local '/custom-close-other-buffer-function)



(/provide)
;;; custom/buffer.el ends here
