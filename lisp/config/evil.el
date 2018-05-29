;;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

;;; Commentary:

;;; Code:

;; Toggle evil-emacs-state with 'ctrl-alt-z', leaving
;; 'ctrl-z' to suspend emacs.
;; This must place before loading evil to make the keymap take effect.
(setq evil-toggle-key "C-M-z")

(eval-when-compile (/require-meta evil))

(require 'evil)
(/require-custom evil)
(/require-lib evil)

;; Set the custom evil initial state for major mode.
(dolist (cus /custom-evil-initial-state-alist)
  (evil-set-initial-state (car cus) (cdr cus)))

;; Don't show state in the echo area.
(setq evil-echo-state nil)

;;; Evil mode line tag
;; Configure the tag string and the place in the mode line.
(/setup-evil-state-tags)
(setq evil-mode-line-format '(before . /mode-line-overwrite-mode))


;;; {{ Text object
;; select Tex mathmode 
(/def-evil-text-object-regexp "$" "\\$" "\\$")
;; select Latex mathmode
(/def-evil-text-object-regexp "m" "\\\\(" "\\\\)")
(/def-evil-text-object-regexp "M" "\\\\\\[" "\\\\\\]")
(/def-evil-text-object-regexp "=" "=" "=")
(/def-evil-text-object-regexp "|" "|" "|")
;; select a line with space at begin and end trimed.
(/def-evil-text-object-regexp "l" "^ *" " *$")
;; select filename
(/def-evil-text-object-thing-at-point f nil filename)
;; select url
(/def-evil-text-object-thing-at-point u nil filename)
;; select defun
(/def-evil-text-object-thing-at-point d nil defun)
;; select space
(/def-evil-text-object-thing-at-point "SPC" nil whitespace)
;; select email
(/def-evil-text-object-thing-at-point "e" nil email)
;;; }}

;;; {{ Evil keymap
;; motion state map is inherit by normal and visual state map.
;; If it isn't override by normal and visual mode, the keys defined
;; here will also has effect in normal and visual mode.
; Emacs style cursor moving
(/remove-keys evil-motion-state-map
  C-f ; evil-scroll-page-down => forward-char 
  C-b ; evil-scroll-page-up   => backward-char 
  C-v ; evil-visual-block     => scroll-up-command
  C-e ; evil-scroll-line-down => move-end-of-line
  ?\t ; evil-jump-forward     => indent-for-tab-command
  )

(/remove-keys evil-normal-state-map
  [?\e ?\.] ; evil-repeat-pop-next => xref-find-definitions
            ; (kbd "M-.") => [134217774] but meta key is represent as ESC
  C-n ; evil-paste-pop-next       => next-line
  C-p ; evil-paste-pop            => previous-line
  )

(/remove-keys evil-insert-state-map
  C-a ; evil-paste-last-insertion => move-beginning-of-line
  C-n ; evil-complete-next        => next-line
  C-p ; evil-complete-previous    => previous-line
  C-k ; evil-insert-digraph       => kill-line
  C-y ; evil-copy-from-above      => yank
  C-r ; evil-paste-from-register  => isearch-backward
  C-v ; quoted-insert             => scroll-up-command
  C-e ; evil-copy-from-below      => move-end-of-line
  )

(/def-keys-evil-state motion
  )

(/def-keys-evil-state normal
  M-n evil-paste-pop-next
  M-p evil-paste-pop
  )

(/def-keys-evil-state insert
  )

;;; leader key
;; - SPC -
(/def-keys-evil-space-leader motion
  SPC /scroll-other-window-transient-mode)

;; - , -
(/def-keys-evil-comma-leader motion
  )

;; - ; -
(/def-keys-evil-semicolon-leader motion
  )

;;; evil-ex-command
(/remove-keys evil-ex-completion-map
  C-a ; evil-ex-completion       => move-beginning-of-line
  C-b ; move-beginning-of-line   => backward-char
  )
(/def-keys evil-ex-completion-map
  M-p previous-complete-history-element
  M-n next-complete-history-element
  )
;;; }}

;;; {{ Common major mode
;; Info-mode
(evil-make-intercept-map Info-mode-map 'motion)

;; help-mode
(evil-make-overriding-map help-mode-map 'motion)

;; package-menu-mode
(/evil-set-initial-state-and-make-override package-menu-mode motion)

;; special-mode
(/evil-set-initial-state-and-make-override special-mode motion)
;;; }}

(evil-mode)

(/provide)
;;; config/evil.el ends here
