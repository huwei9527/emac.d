;;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

;;; Commentary:

;;; Code:

(eval-when-compile (/require-meta hook keymap))

(/require-lib miscellany file)
(/require-custom miscellany)

(setq create-lockfiles nil  ;; Don't use lock files to avoid edit collision.
      ;; Cycle mark ring by repeating C-SPC after C-u C-SPC
      set-mark-command-repeat-pop t
      ;; Delete duplicate item
      history-delete-duplicates t
      ;; Ignore advice redefinition warning message
      ad-redefinition-action 'accept
      ;; Enable primary selection (mid mouse buttion)
      select-enable-primary t
      )

(when (display-graphic-p)
  (require 'pyim)
  (require 'pyim-basedict)
  (pyim-basedict-enable)
  (setq default-input-method "pyim"))

;; always use short form promt
(/advice-add (yes-or-no-p) :override y-or-n-p)

;; camel word support
(subword-mode)

;; disable mouse click
(global-disable-mouse-mode)

;;; {{ Keymap
;; help-map
(/def-keys-ctl-h
 C-v find-variable
 C-f find-function
 C-k find-function-on-key)

;; global-map
(/def-keys-global
 M-/   hippie-expand
 TAB   /tab-dwim
 C-M-v /scroll-other-window-transient-mode
 )
;;; }}

;(display-time-mode 1)

;;; auto-fill
(setq-default auto-fill-function 'do-auto-fill
	      fill-column 80)

;;; Info node
(setq Info-additional-directory-list '("~/.local/share/info/emacs25"))

(/provide)
;;; config/miscellany.el ends here
