;;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

;;; Commentary:

;;; Code:

(/require core lib meta)

(/--defformat advice-add :format "advice-add")
(/--defformat advice-remove :format "advice-remove")
(/--defformat add-hook :format "add-hook")
(/--defformat remove-hook :format "remove-hook")

(/defmacro advice-add (flist where &rest funs)
  "Add advice functions FUNS to functions FLIST.
FLIST is `/--list' of functions.
FUNS are advice functions."
  (declare (indent defun))
  (/--sexp-progn
    (dolist (ad (/--list flist))
      (dolist (fun funs) (/--sexp-exec `(advice-add ',ad ,where #',fun))))))

(/defmacro advice-remove (flist &rest funs)
  "Remove advice functions FUNS from functions FLIST.
FLIST is `/--list' of functions.
FUNS are advice functions."
  (declare (indent defun))
  (/--sexp-progn
    (dolist (ad (/--list flist))
      (dolist (fun funs) (/--sexp-exec `(advice-remove ',ad #',fun))))))

(/defmacro add-hook (hlist &rest funs)
  "Add functions FUNS to hooks HLIST.
HLIST is `/--list' of hooks.
FUNS are functions."
  (declare (indent defun))
  (/--sexp-progn
    (dolist (hk (/--list hlist))
      (dolist (fun funs)
	(/--sexp-exec `(add-hook ',hk #',fun))))))

(/defmacro remove-hook (hlist &rest funs)
  "Remove functions FUNS from hooks HLIST.
HLIST is `/--list' of hooks.
FUNS are functions."
  (declare (indent defun))
  (/--sexp-progn
    (dolist (hk (/--list hlist))
      (dolist (fun funs) (/--sexp-exec `(remove-hook ',hk #',fun))))))

(/defmacro* define-advice-setter (name &rest funs)
  "Construct macro that add/remove advice to/from FUNS.
NAME is a `/--name' argument."
  (declare (indent defun))
  (let* ((name (/--name name)))
    (/--sexp-progn-exec
      `(/defmacro ((advice-add - %s) ,name) (where &rest funs)
	 ,(format "Add advice functions FUNS to %s functions." name)
	 `(/advice-add ,',funs ,where ,@funs))
      `(/defmacro ((advice-remove - %s) ,name) (&rest funs)
	 ,(format "Remove advice functions FUNS from %s functions." name)
	 `(/advice-remove ,',funs ,@funs)))))

(/defmacro* define-hook-setter (name &rest hooks)
  "Construct macro that add/remove hooks to/from hooks HOOKS.
NAME is a `/--name' argument."
  (declare (indent defun))
  (let* ((name (/--name name)))
    (/--sexp-progn-exec
      `(/defmacro ((add-hook - %s) ,name) (&rest funs)
	 ,(format "Add functions FUNS to %s hooks." name)
	 `(/add-hook ,',hooks ,@funs))
      `(/defmacro ((remove-hook - %s) ,name) (&rest funs)
	 ,(format "Remove functions FUNS from %s hooks." name)
	 `(/remove-hook ,',hooks ,@funs)))))

(/defmacro advice-add-false (&rest funs)
  "Add `/false' advice to all functions in FUNS."
  `(/advice-add ,funs :override /false))

(/defmacro advice-remove-false (&rest funs)
  "Remove `/false' advice from all functions in FUNS."
  `(/advice-remove ,funs /false))

(/defmacro advice-add-true (&rest funs)
  "Add `/true' advice to all functions in FUNS."
  `(/advice-add ,funs :override /true))

(/defmacro advice-remove-true (&rest funs)
  "Remove `/true' advice from all functions in FUNS."
  `(/advice-remove ,funs /true))

(/--define-advice-setter buffer-change switch-to-buffer)
(/--define-advice-setter window-switch other-window
			 windmove-left windmove-right windmove-up windmove-down)
(/--define-hook-setter focus-loss focus-out-hook suspend-hook
		       evil-normal-state-entry-hook)

(/defconst* verbose-functions '(message prin1 princ) "Verbose function list.")
(/defmacro progn-silently (&rest body)
  "Evaluate BODY silently by advising `/--verbose-functions' to `ignore'."
  (declare (indent defun))
  (/--sexp-progn
    (/--sexp-exec `(/advice-add-false ,@/--verbose-functions))
    (dolist (form body) (/--sexp-exec form))
    (/--sexp-exec `(/advice-remove-false ,@/--verbose-functions))))

(/defmacro with-no-message (&rest body)
  "Evaluate BODY without message to the minibuffer.
Message still logged in the *Messages* buffer."
  (declare (indent defun))
  (/--sexp-progn-exec `(let* ((inhibit-message t)) ,@body)))

(/defmacro advice-add-silence (&rest funs)
  "Make functions in FUNS execute with no message."
  `(/advice-add ,funs :around /mute-call))

(/defmacro advice-remove-silence (&rest funs)
  "Released the muted functions in FUNS."
  `(/advice-remove ,funs /mute-call))

(/defconst* old-y-or-n-p (symbol-function #'y-or-n-p)
  "Function value of `y-or-n-p' functions.")
(/defconst* old-yes-or-no-p (symbol-function #'yes-or-no-p)
  "Function value of `yes-or-no-p' functions.")
(/defconst* old-true (symbol-function #'/true)
  "Function value of `/true'")
(/defconst* old-false (symbol-function #'/false)
  "Function value of `/false'")

(/defmacro with-yes (&rest body)
  "Inhibit `y-or-n-p' and `yes-or-no-p' function when evaluating BODY.
The two functions won't promot and will always return t."
  (declare (indent defun))
  (/--sexp-progn
    (/--sexp-exec
      `(fset #'y-or-n-p ,/--old-true)
      `(fset #'yes-or-no-p ,/--old-true))
    (/--sexp-append body)
    ;(dolist (form body) (/--sexp-exec form))
    (/--sexp-exec
      `(fset #'y-or-n-p ,/--old-y-or-n-p)
      `(fset #'yes-or-no-p ,/--old-yes-or-no-p))))

(/defmacro with-no (&rest body)
  "Inhibit `y-or-n-p' and `yes-or-no-p' function when evaluating BODY.
The two functions won't promot and will always return nil."
  (declare (indent defun))
  (/--sexp-progn
    (/--sexp-append
      `(fset #'y-or-n-p ,/--old-false)
      `(fset #'yes-or-no-p ,/--old-false))
    (/--sexp-append body)
    ;(dolist (form body) (/--sexp-append-1 form))
    (/--sexp-append
      `(fset #'y-or-n-p ,/--old-y-or-n-p)
      `(fset #'yes-or-no-p ,/--old-yes-or-no-p))))

(/provide)
;;; meta/hook.el ends here
