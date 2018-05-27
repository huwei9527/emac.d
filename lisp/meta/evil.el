;;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

;;; Commentary:

;;; Code:

(eval-when-compile (/require-meta core hook ui keymap))

(defvar /--evil-state-list
  '(insert normal visual emacs motion replace operator)
  "Evil states list.")

(defun /--intern-evil-state (state)
  "Intern evil state symbol `evil-STATE-state'
See `/--intern-format'."
  (/--intern-format "evil-%s-state" (/--name state)))

(defun /--intern-evil-hook (state &optional exit)
  "Intern a evil state entry or exit hook for STATE.
If exit is non-nil, return a exit state hook.
Otherwise return a entry state hook."
  (if exit
      (/--intern-format "%s-%s-hook" (/--intern-evil-state state) "exit")
    (/--intern-format "%s-%s-hook" (/--intern-evil-state state) "entry")))

(defvar /--evil-entry-hook-list
  (mapcar (lambda (state) (/--intern-evil-hook state)) /--evil-state-list)
  "Evil state entry hook list.")

(defvar /--evil-exit-hook-list
  (mapcar (lambda (state) (/--intern-evil-hook state 'exit)) /--evil-state-list)
  "Evil state entry exit list.")


(defmacro /add-hook--evil-state (fun exit &optional st)
  "Add function FUN to evil state hook.
If ST is nil, add to all the evil state hook.
If exit is nil, add to entry state hook, otherwise add to exit state hook."
  (declare (indent defun))
  (let* ((list (if st `(,(/--intern-evil-hook st exit))
		 (if exit /--evil-exit-hook-list
		   /--evil-entry-hook-list))))
    `(/add-hook ,list ,fun)))

(defmacro /add-hook-evil-state-entry (fun &optional st)
  "Add function FUN to evil state ST entry hook.
If ST is nil, add to all the evil state entry hook."
  (declare (indent defun))
  `(/add-hook--evil-state ,fun nil ,st))

(defmacro /add-hook-evil-state-exit (fun &optional st)
  "Add function FUN to evil state ST exit hook.
If ST is nil, add to all the evil state exit hook."
  (declare (indent defun))
  `(/add-hook--evil-state ,fun 'exit ,st))


;;; {{ Evil mode line tag
(defvar /--evil-mode-line-tag-alist
  '((normal .   (:tag "N" :color black))
    (insert .   (:tag "I" :color red))
    (motion .   (:tag "M" :color blue))
    (visual .   (:tag "V" :color green))
    (emacs .    (:tag "E" :color cyan))
    (operator . (:tag "O" :color yellow))
    (replace .  (:tag "R" :color magenta)))
  "Evil mode line tag face configuration alist.")

(defun /--intern-evil-mode-line-face (state)
  "Intern the evil mode line face symbol."
  (/--intern "mode-line-evil-%s-state-tag" (/--name state)))

(defmacro /defface-evil-mode-line-tag ()
  "Define faces for evil to used in the mode line showing the current state."
  (declare (indent defun))
  (/--sexp-progn
    (dolist (attrs /--evil-mode-line-tag-alist)
      (let* ((state (car attrs))
	     (color (plist-get (cdr attrs) :color)))
	(/--sexp-append-1
	  `(defface ,(/--intern-evil-mode-line-face state)
	     '((t (:inherit (,(if (eq state 'normal)
				  (/--intern-face color)
				(/--intern-face 'black color))
			     bold))))
	     ,(format "evil-%s-state face used in tag in mode line."
		      (/--name state))))))))

(defmacro /setup-evil-state-tags ()
  "Set the face of evil mode line tag."
  (declare (indent defun))
  (/--sexp-setq
    (dolist (attrs /--evil-mode-line-tag-alist)
      (let* ((state (car attrs)))
	(/--sexp-pair (/--intern-format "evil-%s-state-tag" state)
		      (propertize (plist-get (cdr attrs) :tag)
				  'face
				  (/--intern-evil-mode-line-face state)))))))
;;; }}

;;; {{ Text objext
;; https://stackoverflow.com/questions/18102004/emacs-evil-mode-how-to-create-a-new-text-object-to-select-words-with-any-non-sp
(defmacro /def-evil-text-object (key start end)
  "Define new text object and bind it to text object map."
  (declare (indent defun))
  (let* ((inner (make-symbol "inner")) (outer (make-symbol "outer")))
    (/--sexp-progn-exec
      `(evil-define-text-object ,inner (cnt &optional beg end type)
	 (evil-select-paren ,start ,end beg end type cnt nil))
      `(evil-define-text-object ,outer (cnt &optional beg end type)
	 (evil-select-paren ,start ,end beg end type cnt t))
      `(define-key evil-inner-text-objects-map ,key ',inner)
      `(define-key evil-outer-text-objects-map ,key ',outer))))
;;; }}

;;; {{ Key map

(defun /--intern-evil-state-map (state)
  "Intern evil state map symbol `evil-STATE-state-map'.
See `/--intern-format'."
  (/--intern-format "%s-map" (/--intern-evil-state state)))

(defun /--state-list (states)
  "Return uniform format of states.
If states is a symbol, return (list 'STATES).
If states is nil, return `/--evil-state-list'.
If states is list, return itself.
Otherwise return nil."
  (if (listp states)
      (or states (setq states /--evil-state-list))
    (setq states (if (symbolp states) `(,states) nil)))
  states)

(defmacro /def-keys-evil-state (states &optional prefix &rest bindings)
  "Define evil key bindings in BINDINGS in `evil-STATE-state-map'.
If STATES is not a list, setup key bindings in that STATES map.
If STATES is a list, setup key bindigns for all state map in the list.
If STATES is nil, setup key bindings for all state map in `/--evil-state-list'.
See `/def-keys' for PRIFIX and BINDINGS useage."
  (declare (indent defun))
  (/--sexp-def-key
    `(evil-define-key ',(/--state-list states) 'global
       ,(pop bindings) ,(pop bindings) ,@bindings)))

(defmacro /def-keys-evil-state-active
    (states keymap &optional prefix &rest bindings)
  "Define evil key bindings for active KEYMAP.
When KEYMAP is active, the defined evil key bindings is also active.
This is done using `evil-define-key'.
See `/def-keys-evil-state' for argument usage."
  (declare (indent defun))
  (/--sexp-def-key
    `(evil-define-key ',(/--state-list states) ,keymap
       ,(pop bindings) ,(pop bindings) ,@bindings)))

(defmacro /def-keys-evil-state-mode
    (states mode &optional prefix &rest bindings)
  "Define evil key bindings for minor mode MODE.
The key bindings are active when the minor mode is active.
This is done using `evil-defin-minor-mode-key'.
See `/def-keys-evil-state' form argument usage."
  (declare (indent defun))
  (/--sexp-def-key
    `(evil-define-minor-mode-key ',(/--state-list states) ',mode
       ,(pop bindings) ,(pop bindings) ,@bindings)))

(defvar /--evil-leader-key-alist
  `((SPC . (:name space     :state (motion normal)))
    ("," . (:name comma     :state motion))
    (";" . (:name semicolon :state motion)))
  "Evil leader key alist.")

(defun /--intern-evil-leader-keymap (state name)
  (declare (indent defun))
  (:documentation
   (format "Intern evil leader keymap `%s-NAME-map'.
See `/--intern'." (/--intern-evil-state 'STATE)))
  (/--intern "%s-%s-leader-map" (/--intern-evil-state state) (/--name name)))

(defun /--intern-evil-leader-key-setter (name)
  "Intern evil leader key setter `/def-keys-evil-NAME-leader'
See `/--intern'."
  (declare (indent defun))
  (/--intern "def-keys-evil-%s-leader" (/--name name)))

(defmacro /def-evil-leader-keys ()
  "Define evil leader keys according to `/--evil-leader-key-alist'."
  (declare (indent defun))
  (/--sexp-progn
    (dolist (attrs /--evil-leader-key-alist)
      (let* ((plist (cdr attrs)) keymap)
	(dolist (state (/--state-list (plist-get plist :state)))
	  (setq keymap (/--intern-evil-leader-keymap state
			 (plist-get plist :name)))
	  (/--sexp-exec
	    `(define-prefix-command ',keymap)
	    `(/def-keys-evil-state ,state ,(car attrs) ,keymap)))))))

(defmacro /--def-evil-leader-key-setters ()
  "Define macro to setup key bindings for each leader key."
  (declare (indent defun))
  (/--sexp-progn
    (dolist (attrs /--evil-leader-key-alist)
      (let* ((name (plist-get (cdr attrs) :name)))
	(/--sexp-exec
	  `(defmacro ,(/--intern-evil-leader-key-setter name)
	       (states &optional prefix &rest bindings)
	     ,(format "Define evil %s leader key bindings." name)
	     (declare (indent defun))
	     (/--sexp-progn
	       (setq states (/--state-list states))
	       (dolist (state states)
		 (/--sexp-exec
		   `(/def-keys
		      ,(/--intern-evil-leader-keymap state ',name)
		      ,prefix ,@bindings))))))))))

(/--def-evil-leader-key-setters)
;;; }}

(defmacro /evil-set-initial-state-and-make-override (mode state)
  "Set the evil initial state of major mode MODE as STATE and make its
  keymap as evil overriding map."
  (declare (indent defun))
  (/--sexp-progn-exec
    `(evil-set-initial-state ',mode ',state)
    `(evil-make-overriding-map ,(/--intern-mode-map mode) ',state)))



(/provide)
;;; meta/evil.el ends here
