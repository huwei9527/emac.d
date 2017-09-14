;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

;; Toggle evil-emacs-state with 'ctrl-alt-z', leaving
;; 'ctrl-z' to suspend emacs.
;; This must place before loading evil to make the keymap take effect.
(setq evil-toggle-key "C-M-z")

(require 'evil)
(require 'evil-custom)
(require 'evil-lib)
(require 'util-lib)

(eval-when-compile
  (require 'keymap-code)
  (require 'hook-code)
  (require 'evil-code))

;;; Set the custom evil initial state for major mode.
(dolist (cus evil-custom-initial-state-alist)
  (evil-set-initial-state (car cus) (cdr cus)))

;; Don't show state in the echo area.
(setq evil-echo-state nil)

;;; Evil mode line tag
;; Configure the tag string and the place in the mode line.
(code-set-face-for-evil-state-tags)
(setq evil-mode-line-format '(before . mode-line-overwrite-mode))

;; operation can take place to nonspace line beginning
(evil-declare-motion 'back-to-indenttation)

(evil-mode 1)


;;; {{ New text object
(code-define-text-object "$" "$" "$")
(code-define-text-object "=" "=" "=")
(code-define-text-object "|" "|" "|")
(code-define-text-object "l" "^ *" " *$")
(evil-define-text-object evil-inner-text-object-path
  (count &optional beg end type)
  "File name of a path"
  (let* ((region (path-at-point))
         (pos-beg (nth 0 region))
         (pos-mid (nth 1 region))
         (pos-end (nth 2 region)))
    (when pos-beg
      (when pos-mid
        (setq pos-beg pos-mid))
      (evil-range pos-beg pos-end :expanded t))))
(evil-define-text-object evil-outer-text-object-path
  (count &optional beg end type)
  "Total path including file name."
  (let* ((region (path-at-point))
         (pos-beg (nth 0 region))
         (pos-mid (nth 1 region))
         (pos-end (nth 2 region)))
    (when pos-beg
      (evil-range pos-beg pos-end :expanded t))))
(code-define-key evil-inner-text-objects-map nil "f" evil-inner-text-object-path)
(code-define-key evil-outer-text-objects-map nil "f" evil-outer-text-object-path)
;; }}

;;; {{ Custom keymap
;; Motion state keymap
(code-defkey-evil-global
 motion
 ;; Use Emacs navigation style in non-insert state.
 "C-f" evil-forward-char
 "C-b" evil-backward-char
 "C-v" evil-scroll-page-down
 )

;; delete evil original key binding to reveal global key binding
(code-remove-key evil-motion-state-map ?\t)

;; Insert state keymap
(code-defkey-evil-global
 insert
 ;; Use Emacs navigation style in insert state.
 "C-a" move-beginning-of-line
 "C-n" evil-next-line
 "C-p" evil-previous-line
 "M-n" evil-complete-next
 "M-p" evil-complete-previous
 "C-y" yank)

;; Normal state keymap
(code-defkey-evil-global
 normal
 "M-n" nil
 "M-p" nil)

;; Motion and insert keymap
(code-defkey-evil-global
 (motion insert)
 "C-e" move-end-of-line
 )

;; Evil all state keymap

;; space 'SPC' leader keymap
(code-defkey-evil-space-key
 nil
 "SPC" scroll-other-window-mode
 "k" scroll-other-window-up
)

;; comma ',' leader keymap
(code-defkey-evil-comma-key
 nil
 "a" code-test-key-binding)

;; semicolon ';' leader keymap
(code-defkey-evil-semicolon-key
 nil
 "SPC" scroll-other-window
 "k" scroll-other-window-up
 "c" path-at-point
 "d" nil
 )

;; keymap in evil-ex-command ':'
(code-define-key
 evil-ex-completion-map nil
 "C-a" move-beginning-of-line
 "C-b" evil-backward-char
 "M-p" previous-complete-history-element
 "M-n" next-complete-history-element)

;; Emacs minibuffer keymap like 'M-x', 'M-:', 'C-x b'
(code-define-key
 minibuffer-local-map nil
 "C-w" evil-delete-backward-word)

;; Restore 'l' and 'h' in 'Info-mode'
(code-defkey-evil-state-map
 motion Info-mode-map
 "l" Info-history-back
 "h" Info-help)

;; Make help-mode override motion state map.
(code-eval-after-load
 help-mode
 (evil-make-overriding-map help-mode-map 'motion))

;; Make package-menu-mode override motion state map.
(code-eval-after-load
 package
 (evil-make-overriding-map package-menu-mode-map 'motion))
;; }}

(provide 'config-evil)
; config.el ends here
