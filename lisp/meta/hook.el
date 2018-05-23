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

(defmacro /advice-add-false (flist)
  "Add `/false' advice to all functions in FLIST."
  `(/advice-add ,flist :override /false))

(defmacro /advice-remove-false (flist)
  "Remove `/false' advice from all functions in FLIST."
  `(/advice-remove ,flist /false))

(defmacro /advice-add-true (flist)
  "Add `/true' advice to all functions in FLIST."
  `(/advice-add ,flist :override /true))

(defmacro /advice-remove-true (flist)
  "Remove `/true' advice from all functions in FLIST."
  `(/advice-remove ,flist /true))

(/--def-advice-setter buffer-change switch-to-buffer)
(/--def-advice-setter window-switch other-window
		      windmove-left windmove-right windmove-up windmove-down)
(/--def-hook-setter focus-loss focus-out-hook suspend-hook
		    evil-normal-state-entry-hook)

(/provide)
;;; meta/hook.el ends here
