;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

(require 'miscellany-custom)
(require 'miscellany-lib)
(require 'file-lib)
(eval-when-compile
  (require 'hook-code)
  (require 'keymap-code))


(setq ;; Don't use lock files. (Don't create .#xxxx file)
      create-lockfiles nil
      ;; Repeat type C-SPC after C-u C-SPC will cycle the mark ring.
      set-mark-command-repeat-pop t
      ;; Do not allow duplicated item.
      history-delete-duplicates t
      ;; Creating new file needs confirmation.
      confirm-nonexistent-file-or-buffer t
      ;; Ingnore ad redefinition warning message
      ad-redefinition-action 'accept
      ;; Enable primary selected (mid mouse button selection)
      x-select-enable-primary t
      ;; Set the default abbrev file.
      abbrev-file-name
      (expand-file-name "abbrev_defs" config-abbrev-directory)
      ;; File to store saveplace configuration
      save-place-file
      (expand-file-name "places" config-saveplace-directory)
      ;; File to store smex configuration
      smex-save-file
      (expand-file-name "smex-items" config-smex-directory)
      ;; File to store ido configuration
      ido-save-directory-list-file
      (expand-file-name "ido.last" config-ido-directory)
      ;;; {{ recentf-mode
      ;; File to store recentf mode configuration
      recentf-save-file
      (expand-file-name "recentf" config-recentf-directory)
      recentf-max-saved-items 1024
      recentf-exclude `("\\.el\\.gz\\'"
			"\\.\\(log\\|aux\\|rip\\)\\'"
			"\\`_.*"
			,config-packages-directory)
      ;; }}
      )

(code-add-advice
 (yes-or-no-p)
 :override
 y-or-n-p)

;;; Camel word surpport
(subword-mode 1)

;;; save exit point
(require 'saveplace)
(setq-default save-place t)

;; {{ Enable X selection in tty terminal
(code-add-advice
 (x-select-text)
 :around
 x-select-text-around-advice)

(code-add-advice
 (x-selection-value)
 :around
 x-selection-value-around-advice)
;;; }}


;;; Read only file.
(code-add-hook
 (find-file-hook)
 (lambda (&rest)
   "Automatically set files in list read-only."
   (catch 'tag-break
     (when (uneditable-file-tail-regexp-p (buffer-file-name))
       (read-only-mode 1)
       (throw 'tag-break nil))
     (dolist (dir file-custom-read-only-directory-list)
       (when (file-in-directory-p buffer-file-name dir)
	 (read-only-mode 1)
	 (throw 'tag-break nil)))
     (dolist (fn file-custom-read-only-file-list)
       (when (string= buffer-file-name (expand-file-name fn))
	 (read-only-mode 1)
	 (throw 'tag-break nil))))))

;; Ignore certain error.
(setq command-error-function
      (lambda (data context caller)
        "Ignore 'text-read-only' error in minibuffer."
        (let* ((error-type (car data)))
          ;; (message "error: %s %d %s | %s | %s" data (length data) (cadr data) context caller)
          (unless (or
                   ;; text-read-only error in minibuffer, <backspace> left margin
                   (and (eq error-type 'text-read-only) (minibufferp))
                   ;; beginning-of-buffer and end-of-buffer error
                   (eq error-type 'beginning-of-buffer)
                   (eq error-type 'end-of-buffer)
                   (eq error-type 'beginning-of-line)
                   (eq error-type 'end-of-line)
                   (and (eq error-type 'user-error)
                        ;; beginning and end of history error
                        (and (minibufferp)
                             (let* ((error-string (cadr data)))
                               (or (string-prefix-p "Beginning of history;"
                                                    error-string)
                                   (string-prefix-p "End of history;"
                                                    error-string))))))
            (command-error-default-function data context caller)))))

;; Remove the message print by help-mode: 'Type C-x-1 ...'
(code-add-advice-ignore (help-window-display-message))

;;; Global key bindings

;; help-map: 'c-h'
(code-defkey-ctl-h
 ;; find in source code
 "C-v" find-variable
 "C-f" find-function
 "C-k" find-function-on-key)

;; goto-map: 'M-g'
(code-defkey-meta-g
 "a" code-test-key-binding)

;; mode-specific-map: 'c-c'
(code-defkey-ctl-c
 [unknown] nil)

;; global key bindings.
(code-defkey-global
 nil
 "C-q" super-switch-buffer
 "TAB" super-tab
 "<backtab>" mode-line-other-buffer
 "<C-tab>" mode-line-other-buffer
 "M-/" hippie-expand
 "M-1" super-close-window
 "M-2" super-split-window
 "M-3" super-switch-window
 )

;; (display-time-mode 1)
(setq-default auto-fill-function 'do-auto-fill)

;; add texlive info node
(add-to-list 'Info-directory-list "/usr/local/texlive/2017/texmf-dist/doc/info")

(provide 'config-miscellany)
; config-miscellany.el ends here
