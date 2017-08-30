;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

(defmacro test-debug (&rest body)
  "If 'test-debug-flag' is t, execute BODY. Otherwise, return nil."
  (declare (indent defun))
  (if test-debug-flag
      `(progn ,@body)
    nil))

(defmacro pp-format (ft &rest args)
  "Extend 'pp' with format FT string.

\(pp (format FT ...))"
  (test-debug
   `(pp (format ,ft ,@args))))

(defmacro pp-macroexpand (sexp)
  "Print the result of macroexpand.

\(pp (macroexpand sexp))"
  (test-debug
   `(pp (macroexpand ',sexp))))

(defmacro pp-macroexpand-all (sexp)
  "Print the result of macroexpand-all.

\(pp (macroexpand-all))"
  (test-debug
   `(pp (macroexpand-all ',sexp))))

(provide 'test-code)
;; test-code.el ends here.