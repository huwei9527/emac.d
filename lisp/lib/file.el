;;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

;;; Commentary:

;;; Code:

(/require-custom file)

(defun /dotdirectoryp (path)
  "Return non-nil if PATH is a system dot directory, otherwise return nil."
  (string-match-p /custom-dotdirectory-regexp
                  (file-name-nondirectory (directory-file-name path))))

; (print (/dotdirectoryp "aaa"))

(defun /subdirectory-1 (path)
  "Return list of the sub-directories of path at depth 1."
  (let* ((dirs nil))
    (dolist (fn (directory-files path 'full))
      (when (file-directory-p fn)
        (unless (/dotdirectoryp fn)
          (push fn dirs))))
    dirs))

(defun /subdirectory (path &optional depth)
  "Return the list of the sub-directories in path at depth at most DEPTH.
PATH itself is excluded. If DEPTH is not a positive interger, the
  whole directory tree will be searched."
  (let* ((dirs nil)
         (dirs-par (list path))
         (dirs-ch nil))
    (unless (and (integerp depth) (> depth 0))
      (setq depth -1))
    (while dirs-par
      (setq dirs-ch nil)
      (dolist (fn-par dirs-par)
        (dolist (fn-ch (/subdirectory-1 fn-par))
          (push fn-ch dirs)
          (push fn-ch dirs-ch)))
      (setq dirs-par (if (eq (setq depth (1- depth)) 0) nil dirs-ch)))
    dirs))

(defun /add-subdirectory-to-list (path list &optional depth)
  "Add the sub-directories of PATH of depth at most DEPTH to LIST.
This function use `add-to-list' to add element to LIST.
This function doesn't add PATH itself.
If DEPTH is not a positive integer, the whole directory tree is searched."
  (let* ((path (file-name-as-directory path)))
    (dolist (fn (/subdirectory path depth))
      (add-to-list list fn))))

(defun /add-directory-to-list (path list &optional depth)
  "Add the directories of PATH of depth at most DEPTH to LIST.
This function use `add-to-list' to add element to LIST.
The PATH itself is also added.
If DEPTH is not a positive integer, the whole directory tree is searched."
  (let* ((path (file-name-as-directory path)))
    (add-to-list list path)
    (/add-subdirectory-to-list path list depth)))

(/provide)
;;; lib/file.el ends here
