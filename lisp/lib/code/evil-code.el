;; -*- lexical-binding : t byte-compile-dynamic : t -*-

(eval-when-compile
  (require 'code)
  (require 'keymap-code))

;;; {{ Evil entry and exit hook utility
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
       (code-item (code--add-evil-state-hook-form hk-fun type st))
     (dolist (cus evil-custom-mode-line-alist)
       (code-item (code--add-evil-state-hook-form hk-fun type (car cus)))))))

(defmacro code-add-evil-entry-hook (hk-fun &optional st)
  "Add hook HK-FUN to the evil-ST-state-entry-hook.

A wrapper for (code--add-evil-hook hk-fun \"entry\" st)."
  `(code--add-evil-hook ,hk-fun "entry" ,st))

(defmacro code-add-evil-exit-hook (hk-fun &optional st)
  "Add hook HK-FUN to the evil-ST-state-exit-hook.

A wrapper for (code--add-evil-hook hk-fun \"exit\" st)"
  `(code--add-evil-hook ,hk-fun "exit" ,st))
;; }}

;;; {{ evil tags in mode line
(defvar code-evil-custom-state-config
  `((normal .   (:tag "N" :color black))
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
     (code-item
      (let* ((st (car state))
             (st-name (symbol-name st))
             (cr (plist-get (cdr state) :color)))
        `(defface ,(intern-format "evil-custom-%s-state-tag" st-name)
           '((t (:inherit (,(intern-format (if (eq st 'normal)
                                               "%s-foreground"
                                             "black-%s") (symbol-name cr))
                           bold))))
           ,(format "The face of 'evil-%s-state-tag' used in mode-line."
                    st-name)))))))

(defmacro code-set-face-for-evil-state-tags ()
  "Set the face of evil tags in mode line."
  (code-setq
   (dolist (state code-evil-custom-state-config)
     (let* ((st (car state))
            (st-name (symbol-name st))
            (st-plist (cdr state))
            (tag-name (plist-get st-plist :tag))
            (cr (plist-get st-plist :color)))
       (code-pair (intern-format "evil-%s-state-tag" st-name)
                  (propertize tag-name
                              'face (intern-format "evil-custom-%s-state-tag"
                                                   st-name)))))))
;; }}

;;; {{ Evil keymap utilities
(defun code-evil-leader-keymap-name (state key-name)
  "The keymap name for leader key KEY-NAME"
  (format "evil-%s-state-%s-map" (symbol-name state) key-name))

(defvar code-evil-custom-prefix-key-config
  `((evil-motion-state-map
     . (("SPC" . (:name "space" :keymap
                        ,(intern (code-evil-leader-keymap-name
                                  'motion "space"))))
        (","   . (:name "comma" :keymap
                        ,(intern (code-evil-leader-keymap-name
                                  'motion "comma"))))
        (";"   . (:name "semicolon" :keymap
                        ,(intern (code-evil-leader-keymap-name
                                  'motion "semicolon")))))))
  "Evil leader key config")

(defmacro code-defkey-evil-global (state key def &rest bindings)
  "Define evil global key."
  (declare (indent defun))
  (code-progn
   (cond
    ((symbolp state)
     (code-item
      `(code-define-key
        ,(intern-format "evil-%s-state-map" (symbol-name state))
        nil ,key ,def ,@bindings)))
    ((listp state)
     (dolist (st state)
       (code-item
        `(code-define-key
          ,(intern-format "evil-%s-state-map" (symbol-name st))
          nil ,key ,def ,@bindings))))
    (t
     (error "Invalid state: %s" state)))))

(defmacro code-defkey-evil-state-map-raw (state keymap key def &rest bindings)
  "Define evil key map for special mode map"
  (declare (indent defun))
  (code-sexp
   (code-append 'evil-define-key `(quote ,state) keymap
                (code-make-key key) `(function ,def))
   (while bindings
     (code-append (code-make-key (pop bindings)) `(function ,(pop bindings))))))

(defmacro code-defkey-evil-state-map (state keymap key def &rest bindings)
  "Define evil key map for special mode map."
  (declare (indent defun))
  (code-progn
   (cond
    ((symbolp state)
     (code-item
      `(code-defkey-evil-state-map-raw ,state ,keymap ,key ,def ,@bindings)))
    ((listp state)
     (dolist (st state)
       (code-item
        `(code-defkey-evil-state-map-raw ,st ,keymap ,key ,def ,@bindings))))
    (t
     (error "Invalid state: %s" state)))))

(defmacro code-defkey-evil-prefix-key ()
  "Define evil leader keymap."
  (declare (indent defun))
  (code-progn
   (dolist (keymap-list code-evil-custom-prefix-key-config)
     (let* ((keymap (car keymap-list)))
       (dolist (key-list (cdr keymap-list))
         (let* ((key (car key-list))
                (key-plist (cdr key-list))
                (prefix-map (plist-get key-plist :keymap)))
           (code-item
            `(define-prefix-command ',prefix-map)
            `(code-define-key ,keymap nil ,key ,prefix-map))))))))

(defmacro code-defsetter-evil-leader-key ()
  "Define key setter for evil leader map."
  (code-progn
   (let* ((keymap-list (assq 'evil-motion-state-map
                             code-evil-custom-prefix-key-config)))
     (dolist (key-list (cdr keymap-list))
       (let* ((key (car key-list))
              (key-plist (cdr key-list))
              (key-name (plist-get key-plist :name))
              (prefix-map (plist-get key-plist :keymap)))
         (code-item
          `(defmacro ,(intern-format "code-defkey-evil-%s-key" key-name)
               (state key def &rest bindings)
             ,(format "Define evil %s leader keymap" key-name)
             (code-progn
              (if (or (null state) (eq state 'motion))
                  (code-item
                   `(code-define-key ,',prefix-map
                                     nil ,key ,def ,@bindings))
                (when (or (eq state 'normal)
                          (and (listp state)
                               (memq 'normal state)))
                  (code-item
                   `(code-define-key evil-normal-state-map
                                     ,',key ,key ,def ,@bindings)))
                (when (or (eq state 'visual)
                          (and (listp state)
                               (memq 'visual state)))
                  (code-item
                   `(code-define-key evil-visual-state-map
                                     ,',key ,key ,def ,@bindings))))))))))))

(code-defsetter-evil-leader-key)
;; }}

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
