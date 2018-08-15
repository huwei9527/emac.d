;;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

;;; Commentary:

;;; Code:

(/defun message-test-start ()
  "Show test start message."
  (message "%s: test start." load-file-name))

(/defun message-test-end ()
  "Show test stop message."
  (message "%s: test end." load-file-name))

(/defun hello-world (&rest args)
  "Message `Hello world'."
  (interactive)
  (message "Hello world!"))

(/defun print-arguments (&rest args)
  "Print the arguments."
  (interactive)
  (print args))

(/defvar* test-key-binding-count 0
  "Count the times that invoking `/show-key-binding'")

(/defun message-this-command ()
  "Show this command."
  (message "%s[%d]: (k: %s) (v: %s)"
	   this-command (setq /--test-key-binding-count
			      (1+ /--test-key-binding-count))
	   (this-command-keys) (this-command-keys-vector)))

(/defun show-key-binding (&rest args)
  "Show the key binding invoke this command."
  (interactive)
  (/message-this-command))

(/defun show-key-binding-1 (&rest args)
  "Show the key binding invoke this command."
  (interactive)
  (/message-this-command))

(/defun key-string (keys &optional prefix)
  "Show key string"
  (setq keys (/--key-sequence keys))
  (format "%s %s" keys (key-description keys)))

(/defvar* debug-flag nil "Debug flag.")

(/defun debugp ()
  "Return non-nil if `/--debug-flag' bounded and non-nil."
  (declare (indent defun))
  (and (boundp /--debug-flag) /--debug-flag))

(/defmacro* debug (&rest body)
  "Evaluate BODY if `/--debug-flag' is non-nil."
  (declare (indent defun))
  (if (/--debugp) `(progn ,body) nil))

(/defmacro ppmacroexpand (sexp)
  ""
  (declare (indent defun))
  `(pp (macroexpand ',sexp)))

(/defmacro ppmacroexpand-all (sexp)
  ""
  (declare (indent defun))
  `(pp (macroexpand-all ',sexp)))

(/defun test (name)
  "Load test file."
  (interactive "M")
  (load (expand-file-name name /custom-lisp-test-directory)))

(/define-minor-mode test-minor-mode
  "Test minor mode."
  :init-value nil
  :lighter nil
  :keymap `(("a" . /show-key-binding))
  :global nil
  (if /test-minor-mode (message "/test-minor-mode ON.")
    (message "/test-minor-mode OFF."))
  )

(/defmacro test-char (c fun)
  "Test char-p"
  (declare (indent defun))
  `(message "[%d] %s: %s"
	    (setq cnt (1+ cnt)) ,(/--name c) ,(apply fun (list c))))

(/provide)
;;; lib/test.el ends here
