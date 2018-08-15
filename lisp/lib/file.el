;;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

;;; Commentary:

;;; Code:

(eval-when-compile (/require-meta file hook))
(/require-custom file)
;; (/require-lib core format)

(/defun path (&rest args)
  "Concatnate the ARGS to a path.
The intermediate path in ARGS are `/--name' argument."
  (let* ((dir (if (> (length args) 1) (/--name (pop args)) default-directory)))
    (while args (setq dir (expand-file-name (/--name (pop args)) dir))) dir))

(/defun path-to-file-name (path)
  "Transform path name PATH to file name.
Replace the system directory symbol '/' with '!'."
  (subst-char-in-string ?/ ?! (replace-regexp-in-string "!" "!!" path)))

(/defun file-contain-p (file dir)
  "Return non-nil if FILE is in DIR.
FILE and DIR are `/--name' argument.
FILE and DIR are `expand' in `default-directory' if they are not absolute path."
  (string-prefix-p (file-name-as-directory
		    (file-truename (expand-file-name (/--name dir))))
		   (file-truename (expand-file-name (/--name file)))))

(/defun file-name-match (path regexp)
  "Return non-nil if `/--file-name' of PATH match REGEXP.
PATH is a `/--file-name' argument.
REGEXP is a string of regular expression."
  (string-match-p regexp (/--file-name path)))

(/--define-file-name-predictor)

(/defun subdirectory (path &optional depth)
  "Return the list of the subdirectories in the PATH.
DEPTH is a positive interger which determines the maximal depth
of subdirectories this function will search.
If DEPTH is nil, the whole directory will be seached."
  (let* ((parent (list path)) child dirs)
    (or (and depth (> depth 0)) (setq depth -1))
    (while parent
      (setq child nil)
      (dolist (pf parent)
	(dolist (cf (directory-files pf 'full))
	  (and (file-directory-p cf)
	       (unless (/dotdirectory-file-name-p cf)
		 (push cf dirs)
		 (push cf child)))))
      (setq parent (if (eq (cl-decf depth) 0) nil child)))
    dirs))

(/defun add-subdirectory-to-list (path list &optional depth)
  "Add the sub-directories of PATH of depth at most DEPTH to LIST.
This function use `add-to-list' to add element to LIST.
This function doesn't add PATH itself.
If DEPTH is not a positive integer, the whole directory tree is searched."
  (let* ((path (file-name-as-directory path)))
    (dolist (fn (/subdirectory path depth))
      (add-to-list list fn))))

(/defun add-directory-to-list (path list &optional depth)
  "Add the directories of PATH of depth at most DEPTH to LIST.
This function use `add-to-list' to add element to LIST.
The PATH itself is also added.
If DEPTH is not a positive integer, the whole directory tree is searched."
  (let* ((path (file-name-as-directory path)))
    (add-to-list list path)
    (/add-subdirectory-to-list path list depth)))

(/defun* make-file-tag (create)
  "format colored make file tag."
  (if noninteractive
      (if create (/format-red "C") (/format-green "E"))
    (if create (propertize "C" 'face '/red-foreground)
      (propertize "E" 'face '/green-foreground))))

(/defun make-file-safe (path &optional verbose)
    "Create file named PATH.
If PATH can't be created or PATH is already exits, no error will be signaled.
If VERBOSE is non-nil, show messages."
    (let* (create)
      (unless (file-exists-p path)
	(with-temp-buffer (write-file path))
	(setq create t))
      (when verbose (message "[%s] %s" (/--make-file-tag create) path))))

(/defun make-directory-safe (path &optional verbose)
  "Create directory named PATH.
If PATH can't be created or PATH is already exits, no error will be signaled.
If VERBOSE is non-nil, show messages."
  (let* (create)
    (unless (file-directory-p path)
      (make-directory path)
      (setq create t))
    (when verbose (message "[%s] %s" (/--make-file-tag create) path))))

;;; {{ buffer
(/defun save-buffer (&optional silent)
  "Save the current buffer.
Return t if the save actually performed, otherwise return nil.
If SILENT is nil, avoid message when saving."
  (if (and buffer-file-name			       ;; Vaid buffer
	   (buffer-modified-p)			       ;; Modified
	   (not (/uneditable-file-p buffer-file-name)) ;; Emacs editable
	   (file-writable-p buffer-file-name)	       ;; Write permission
	   )
      (progn
	(if silent (/with-no-message (save-buffer)) (save-buffer))
	t)
    nil))

(/defun save-buffer-all (&optional silent)
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
(/defun char-path-delimiter-p (c)
  "Return t if C is a path delimiter."
  (setq c (/--character c))
  (or (eq ?\/ c) (eq ?\\ c)))

(/defun char-path-p (c)
  "Return t if C is a path constituent."
  (setq c (/--character c))
  (not (or (<= 0 c 32)
	   (memq c /custom-invalid-path-char-list))))
;;; }}

(/provide)
;;; lib/file.el ends here
