;;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

;;; Commentary:

;;; Code:

(eval-when-compile (/require-meta file))
(/require-custom file)
(/require-lib core format)

(defun /path-to-file-name (path)
  "Transform path name PATH to file name.
Replace the system directory symbol '/' with '!'."
  (subst-char-in-string ?/ ?! (replace-regexp-in-string "!" "!!" path)))

(defun /file-or-buffer-name (&rest buf)
  "Return the filename of the buffer BUF.
If no file is related to the buffer, return the buffer name."
  (or buf (setq buf (current-buffer)))
  (if (buffer-file-name buf)
      (file-truename (buffer-file-name buf))
    (buffer-name buf)))

(defun /file-in-directory-p (file dir)
  "Return non-nil if FILE is in DIR.
Subdirectory is also counted."
  (string-prefix-p (file-name-as-directory
		    (file-truename (expand-file-name dir)))
		   (file-truename (expand-file-name file))))

(defsubst /file-name (path)
  "Return the filename of PATH.
If PATH is a directory, the system directory character is ommited."
  (file-name-nondirectory (directory-file-name path)))

(defsubst /file-name-match (path regexp)
  "Return non-nil if the non-directory filename of PATH match REGEXP."
  (string-match-p regexp (/file-name path)))

(/def-file-name-predictor-all)

(defun /subdirectory-1 (path)
  "Return list of the sub-directories of path at depth 1."
  (let* ((dirs nil))
    (dolist (fn (directory-files path 'full))
      (when (file-directory-p fn)
        (unless (/dotdirectory-p fn)
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

(defun /--format-make-file-tag (create)
  "format colored make file tag."
  (if noninteractive
      (if create (/format-red "C") (/format-green "E"))
    (if create (propertize "C" 'face '/red-foreground)
      (propertize "E" 'face '/green-foreground))))

(defun /make-file-safe (path &optional verbose)
    "Create file named PATH.
If PATH can't be created or PATH is already exits, no error will be signaled.
If VERBOSE is non-nil, show messages."
    (let* (create)
      (unless (file-exists-p path)
	(with-temp-buffer (write-file path))
	(setq create t))
      (when verbose (message "[%s] %s" (/--format-make-file-tag create) path))))

(defun /make-directory-safe (path &optional verbose)
  "Create directory named PATH.
If PATH can't be created or PATH is already exits, no error will be signaled.
If VERBOSE is non-nil, show messages."
  (let* (create)
    (unless (file-directory-p path)
      (make-directory path)
      (setq create t))
    (when verbose (message "[%s] %s" (/--format-make-file-tag create) path))))

;;; {{ buffer
(defun /save-buffer (&optional silent)
  "Save the current buffer.
Return t if the save actually performed, otherwise return nil.
If SILENT is nil, avoid message when saving."
  (if (and buffer-file-name			       ;; Vaid buffer
	   (buffer-modified-p)			       ;; Modified
	   (not (/uneditable-file-p buffer-file-name)) ;; Emacs editable
	   (file-writable-p buffer-file-name)	       ;; Write permission
	   )
      (progn
	;; FIXME: slient has no effect
	(save-buffer)
	;; (let* ((inhibit-message silent))
	;;   (save-buffer))
	;(if silent (with-temp-message "" (save-buffer)) (save-buffer))
	t)
    nil))

(defun /save-buffer-all (&optional silent)
  "Save all the buffers.
Return the number of buffers actually saved."
  (let* ((cnt 0))
    (save-excursion
      (dolist (buf (buffer-list))
	(set-buffer buf)
	(and (/save-buffer silent) (setq cnt (1+ cnt)))))
    cnt))
;;; }}

;;; {{
(defun /char-path-delimiter-p (c)
  "Return t if C is a path delimiter."
  (setq c (/--character c))
  (or (eq ?\/ c) (eq ?\\ c)))

(defun /char-path-p (c)
  "Return t if C is a path constituent."
  (setq c (/--character c))
  (not (or (<= 0 c 32)
	   (memq c /custom-invalid-path-char-list))))
;;; }}

(/provide)
;;; lib/file.el ends here
