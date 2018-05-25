;;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

;;; Commentary:

;;; Code:

(defun /message-test-start ()
  "Show test start message."
  (message "%s: test start." load-file-name))

(defun /message-test-end ()
  "Show test stop message."
  (message "%s: test end." load-file-name))

(defun /hello-world (&rest args)
  "Message `Hello world'."
  (interactive)
  (message "Hello world!"))

(defun /print-arguments (&rest args)
  "Print the arguments."
  (interactive)
  (print args))

(defvar /--debug-flag nil "Debug flag.")

(defun /--debugp ()
  "Return non-nil if `/--debug-flag' bounded and non-nil."
  (declare (indent defun))
  (and (boundp /--debug-flag) /--debug-flag))

(defmacro /--debug (&rest body)
  "Evaluate BODY if `/--debug-flag' is non-nil."
  (declare (indent defun))
  (if (/--debugp) `(progn ,body) nil))

(defmacro /ppmacroexpand (sexp)
  ""
  (declare (indent defun))
  `(pp (macroexpand ',sexp)))

(defmacro /ppmacroexpand-all (sexp)
  ""
  (declare (indent defun))
  `(pp (macroexpand-all ',sexp)))

(defun /test (name)
  "Load test file."
  (interactive "M")
  (load (expand-file-name name /custom-lisp-test-directory)))

(/provide)
;;; lib/test.el ends here
