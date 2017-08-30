;; -*- lexical-binding : t byte-compile-dynamic : t -*-

(eval-when-compile
  (require 'code))

;; (require 'util-lib)

(defun code--add-evil-state-hook-form (hk-fun type state)
  "Construct the form to add hook HK-FUN to the evil-STATE-state-TYPE-hook."
  `(add-hook ',(intern-format "evil-%s-state-%s-hook" state type)
             #',hk-fun))

(defmacro code--add-evil-hook (hk-fun type &optional st)
  "Add hook HK-FUN to the evil-ST-state-TYPE-hook.

TYPE can be \"entry\" and \"exit\". ST is the state of the hook. If ST is nil,
add hook HK-FUN to all the state hooks of TYPE."
  (code-progn
   (if st
       (code-push (code--add-evil-state-hook-form hk-fun type st))
     (dolist (cus evil-custom-mode-line-alist)
       (code-push (code--add-evil-state-hook-form hk-fun type (car cus)))))))

(defmacro code-add-evil-entry-hook (hk-fun &optional st)
  "Add hook HK-FUN to the evil-ST-state-entry-hook.

A wrapper for (code--add-evil-hook hk-fun \"entry\" st)."
  `(code--add-evil-hook ,hk-fun "entry" ,st))

(defmacro code-add-evil-exit-hook (hk-fun &optional st)
  "Add hook HK-FUN to the evil-ST-state-exit-hook.

A wrapper for (code--add-evil-hook hk-fun \"exit\" st)"
  `(code--add-evil-hook ,hk-fun "exit" ,st))

(defvar code-evil-custom-state-config
  `((normal .   (:tag "N" :color white))
    (insert .   (:tag "I" :color red))
    (motion .   (:tag "M" :color blue))
    (visual .   (:tag "V" :color green))
    (emacs .    (:tag "E" :color cyan))
    (operator . (:tag "O" :color yellow))
    (replace .  (:tag "R" :color magenta)))
  "Evil state configuration.")

(defmacro code-defface-evil-state-tags ()
  "Define the face for evil state tags."
  (code-progn
   (dolist (state code-evil-custom-state-config)
     (code-push
      (let* ((st (car state))
             (st-name (symbol-name st))
             (cr (plist-get (cdr state) :color)))
        `(defface ,(intern-format "evil-custom-%s-state-tag" st-name)
           '((t (:inherit (,(intern-format "%s-black" (symbol-name cr)) bold))))
           ,(format "The face of 'evil-%s-state-tag' used in mode-line." st-name)))))))

(defmacro code-set-face-for-evil-state-tags ()
  "Set the face of evil tags in mode line."
  (code-progn
   (dolist (state code-evil-custom-state-config)
     (code-push
      `()))))

(defmacro code-define-text-object (key start-regex end-regex)
  "Define new text object and bind it to text object map."
  (let* ((inner-name (make-symbol "inner-name"))
         (outer-name (make-symbol "outer-name")))
    `(progn
       (evil-define-text-object ,inner-name (count &optional beg end type)
         (evil-select-paren ,start-regex ,end-regex beg end type count nil))
       (evil-define-text-object ,outer-name (count &optional beg end type)
         (evil-select-paren ,start-regex ,end-regex beg end type count t))
       (define-key evil-inner-text-objects-map ,key ',inner-name)
       (define-key evil-outer-text-objects-map ,key ',outer-name))))

(provide 'evil-code)
; evil-code.el ends here