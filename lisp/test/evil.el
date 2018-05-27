;;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

;;; Commentary:

;;; Code:

(/require-meta evil)

(defun /evil-show-state (&rest args)
  "Show current evil state"
  (message "%s" evil-state))

(/message-test-start)
(when nil
  (message "%s %s" (/--intern-evil-hook 'normal)
	   (/--intern-evil-hook 'insert 'exit))
  (message "%s" /--evil-entry-hook-list)
  (message "%s" /--evil-exit-hook-list)
  (/add-hook--evil-state /hello-world nil)
  (/add-hook-evil-state-entry /evil-show-state)
  (/add-hook-evil-state-exit /evil-show-state)
  (/defface-evil-mode-line-tag)
  (/ppmacroexpand (/setup-evil-state-tags)))

(when t
  ; (/--def-keys-evil-state motion ab /show-key-binding)
  ; (/def-keys-evil-state normal M-n /show-key-binding)
  ;(/def-keys-evil-state (visual normal) M-n /show-key-binding M-p /show-key-binding)
  ; (/def-keys-evil-state nil M-n /show-key-binding)
  (when nil
    (/def-keys-evil-state-active nil /test-minor-mode-map
      "b" (lambda () (interactive)
	    (message "FUck YoU."))))
  (when nil
    ;; (/def-keys-evil-state-mode motion /test-minor-mode
    ;; 			      "b" /show-key-binding)
    )
  )

(when nil
  (/def-evil-leader-keys)
  (/--def-evil-leader-key-setters)
  (/def-keys-evil-space-leader motion a
			       a /show-key-binding
			       b /show-key-binding
			       )
  (/def-keys-evil-space-leader normal C-c
			       a /show-key-binding-1
			       b /show-key-binding-1)
  )

(when nil
  (/ppmacroexpand (/evil-set-initial-state-and-make-override
  major-mode nil)))

(when nil
  (/def-evil-text-object f nil
    (let* ((region (bounds-of-thing-at-point 'filename)))
      (evil-range (car region) (cdr region)))
    ))

(when nil
  (/def-evil-text-object-thing-at-point f inner url))


(/message-test-end)

(/provide)
;;; test/evil.el ends here
