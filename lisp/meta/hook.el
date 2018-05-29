;;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

;;; Commentary:

;;; Code:

(/require-meta core)
(/require-lib core)

(defmacro /advice-add (flist where &rest funs)
  "Add advice functions in FUNS to all functions in FLIST."
  (declare (indent defun))
  (/--sexp-progn
    (dolist (ad (/--list flist))
      (dolist (fun funs)
	(/--sexp-exec `(advice-add ',ad ,where #',fun))))))

(defmacro /advice-remove (flist &rest funs)
  "Remove advice funtions FUNS from all functions in FLIST."
  (declare (indent defun))
  (/--sexp-progn
    (dolist (ad (/--list flist))
      (dolist (fun funs)
	(/--sexp-exec `(advice-remove ',ad #',fun))))))

(defmacro /add-hook (hlist &rest funs)
  "Add hook functions in FUNS to all hooks in HLIST."
  (declare (indent defun))
  (/--sexp-progn
    (dolist (hk (/--list hlist))
      (dolist (fun funs)
	(/--sexp-exec `(add-hook ',hk #',fun))))))

(defmacro /remove-hook (hlist &rest funs)
  "Remove hook functions in FUNS from all hooks in HLIST."
  (declare (indent defun))
  (/--sexp-progn
    (dolist (hk (/--list hlist))
      (dolist (fun funs)
	(/--sexp-exec `(remove-hook ',hk #',fun))))))

(defmacro /--def-advice-setter (name &rest funs)
  "Construct macro that add and remove advice to/from sets of functions in FUNS."
  (declare (indent defun))
  (let* ((name (/--name name)))
    (/--sexp-progn-exec
      `(defmacro ,(/--intern "advice-add-%s" name) (where &rest funs)
	 ,(format "Add advice functions in FUNS to all %s functions." name)
	 `(/advice-add ,',funs ,where ,@funs))
      `(defmacro ,(/--intern "advice-remove-%s" name) (&rest funs)
	 ,(format
	   "Remove advice functions in FUNS from all %s functions." name)
	 `(/advice-remove ,',funs ,@funs)))))

(defmacro /--def-hook-setter (name &rest hooks)
  "Construct macro that add and remove hooks to/from sets of hooks in HOOKS."
  (declare (indent defun))
  (let* ((name (/--name name)))
    (/--sexp-progn-exec
      `(defmacro ,(/--intern "add-hook-%s" name) (&rest funs)
	 ,(format "Add functions in FUNS to all %s hooks." name)
	 `(/add-hook ,',hooks ,@funs))
      `(defmacro ,(/--intern "remove-hook-%s" name) (&rest funs)
	 ,(format "Remove functions in FUNS from all %s hooks." name)
	 `(/remove-hook ,',hooks ,@funs)))))

(defmacro /advice-add-false (&rest funs)
  "Add `/false' advice to all functions in FUNS."
  `(/advice-add ,funs :override /false))

(defmacro /advice-remove-false (&rest funs)
  "Remove `/false' advice from all functions in FUNS."
  `(/advice-remove ,funs /false))

(defmacro /advice-add-true (&rest funs)
  "Add `/true' advice to all functions in FUNS."
  `(/advice-add ,funs :override /true))

(defmacro /advice-remove-true (&rest funs)
  "Remove `/true' advice from all functions in FUNS."
  `(/advice-remove ,funs /true))

(/--def-advice-setter buffer-change switch-to-buffer)
(/--def-advice-setter window-switch other-window
		      windmove-left windmove-right windmove-up windmove-down)
(/--def-hook-setter focus-loss focus-out-hook suspend-hook
		    evil-normal-state-entry-hook)

(defconst /--verbose-functions '(message) "Verbose function list.")
(defmacro /progn-silently (&rest body)
  "Evaluate BODY silently by advising `/--verbose-functions' to `ignore'."
  (declare (indent defun))
  (/--sexp-progn
    (/--sexp-append-1 `(/advice-add-false ,@/--verbose-functions))
    (dolist (form body) (/--sexp-append-1 form))
    (/--sexp-append-1 `(/advice-remove-false ,@/--verbose-functions))))

(defmacro /with-no-message (&rest body)
  "Evaluate BODY without message to the minibuffer.
Message still logged in the *Messages* buffer."
  (declare (indent defun))
  (/--sexp-progn-exec `(let* ((inhibit-message t)) ,@body)))

(defmacro /advice-add-silence (&rest funs)
  "Make functions in FUNS execute with no message."
  `(/advice-add ,funs :around /call-silently))

(defmacro /advice-remove-silence (&rest funs)
  ""
  `(/advice-remove ,funs /call-silently))

(defconst /--old-y-or-n-p (symbol-function 'y-or-n-p)
  "Definition of `y-or-n-p' functions.")
(defconst /--old-yes-or-no-p (symbol-function 'yes-or-no-p)
  "Definition of `yes-or-no-p' functions.")
(defconst /--old-true (symbol-function '/true) "Defeintion of `/true'")
(defconst /--old-false (symbol-function '/false) "Defeintion of `/false'")

(defmacro /with-yes (&rest body)
  "Inhibit `y-or-n-p' and `yes-or-no-p' function when evaluating BODY.
The two functions won't promot and will always return t."
  (declare (indent defun))
  (/--sexp-progn
    (/--sexp-append
      `(fset 'y-or-n-p /--old-true)
      `(fset 'yes-or-no-p /--old-true))
    (dolist (form body) (/--sexp-append-1 form))
    (/--sexp-append
      `(fset 'y-or-n-p /--old-y-or-n-p)
      `(fset 'yes-or-no-p /--old-yes-or-no-p))))

(defmacro /with-no (&rest body)
  "Inhibit `y-or-n-p' and `yes-or-no-p' function when evaluating BODY.
The two functions won't promot and will always return nil."
  (declare (indent defun))
  (/--sexp-progn
    (/--sexp-append
      `(fset 'y-or-n-p /--old-false)
      `(fset 'yes-or-no-p /--old-false))
    (dolist (form body) (/--sexp-append-1 form))
    (/--sexp-append
      `(fset 'y-or-n-p /--old-y-or-n-p)
      `(fset 'yes-or-no-p /--old-yes-or-no-p))))

(/provide)
;;; meta/hook.el ends here
