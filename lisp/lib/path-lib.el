; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

(defvar dot-directory-regexp
  (concat "\\`" (regexp-opt (list "." "..")) "\\'")
  "The regular expression match exactly the name of dot directories. (. ..)")

(defun dot-directory-p (path)
  "Check whether path is a system dot directory.

Return:
non-nil - yes, nil - no"
  (string-match-p dot-directory-regexp
                  (file-name-nondirectory (directory-file-name path))))

(defun sub-directory-1 (path)
  "Find the sub-directories of PATH (a directory) with depth 1.

Return:
directories list"
  (let ((dirs nil))
    (dolist (fn (directory-files path 'full))
      (when (file-directory-p fn)
        (unless (dot-directory-p fn)
          (push fn dirs))))
    dirs))

(defun sub-directory (path &optional depth)
  "Find the sub-directories of PATH (a directory).

Note: PATH itself is excluded.
The searching depth is DEPTH if DEPTH is a positive integer,
otherwise search the whole directory."
  (let ((dirs nil)
        (dirs-par (list path))
        (dirs-ch nil))
    (unless (and (integerp depth) (> depth 0))
      (setq depth -1))
    (while dirs-par
      (setq dirs-ch nil)
      (dolist (fn-par dirs-par)
        (dolist (fn-ch (sub-directory-1 fn-par))
          ;; We use push instead of add-to-list which is equal because
          ;; we only loop through all the subdirectory once and the
          ;; can't be duplicated. And push is fast.
          (push fn-ch dirs)
          (push fn-ch dirs-ch)))
      (setq dirs-par (if (= (setq depth (1- depth)) 0) nil dirs-ch)))
    dirs))


(defun add-sub-directory-to-list (path list &optional depth)
  "Add PATH (a directory) to LIST using add-to-list function.

Note: PATH itself is excluded.
The searching depth is DEPTH if DEPTH is a positive integer,
otherwise search the whole directory."
  (setq path (file-name-as-directory path))
  (dolist (fn (sub-directory path depth))
    (add-to-list list fn)))

(defun add-directory-to-list (path list &optional depth)
  "Add PATH (a directory) to LIST using add-to-list function.

Note: PATH itself is included.
The searching depth is DEPTH if DEPTH is a positive integer,
otherwise search the whole directory."
  (setq path (file-name-as-directory path))
  (add-to-list list path)
  (add-sub-directory-to-list path list depth))


(provide 'path-lib)
; path-lib.el ends here