;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

(require 'miscellany-custom)
(require 'file-lib)
(eval-when-compile
  (require 'code))

(setq ;; Don't use lock files. (Don't create .#xxxx file)
      create-lockfiles nil
      ;; Repeat type C-SPC after C-u C-SPC will cycle the mark ring.
      set-mark-command-repeat-pop t
      ;; Set the default abbrev file.
      abbrev-file-name (expand-file-name config-abbrev-directory)
      ;; Do not allow duplicated item.
      history-delete-duplicates t
      ;; Creating new file needs confirmation.
      confirm-nonexistent-file-or-buffer t
      ;; Ingnore ad redefinition warning message
      ad-redefinition-action 'accept
      ;; Enable primary selected (mid mouse button selection)
      x-select-enable-primary t
      )


;; Set default font size in gui frame
(set-face-attribute 'default nil :height 120)

; Read only file.
(add-hook 'find-file-hook
          (lambda (&rest)
            "Automatically set files in list read-only."
            (catch 'tag-break
              (when (file-custom-file-tail-filtered-p (buffer-file-name))
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

;; global key bindings.
(code-defkey-global
 nil
 "C-q" next-buffer
 "<backtab>" mode-line-other-buffer
 "M-/" hippie-expand
 )



;; (which-func-mode 1)
;; (display-time-mode 1)

(provide 'config-miscellany)
; config-miscellany.el ends here
