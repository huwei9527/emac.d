;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

(require 'evil-custom)
(eval-when-compile (require 'evil-code))

(defun evil-print-state ()
  "Print current evil state."
  (cond
   ((evil-insert-state-p) "insert")
   ((evil-normal-state-p) "normal")
   ((evil-emacs-state-p) "emacs")
   ((evil-motion-state-p) "motion")
   ((evil-replace-state-p) "replace")
   ((evil-operator-state-p) "operator")
   (t "unknown")))


(code-defsetter-evil-mode-line-state-all)
(code-defsetter-evil-mode-line)

(setq scnt 0)
(advice-add 'switch-to-buffer :after (lambda (&rest args) ""
                                       (evil-custom-set-mode-line)
                                       (message "sb %s %s %s %s" (window-minibuffer-p) (current-buffer) (selected-window) (evil-print-state))))
(advice-add 'select-window :after (lambda (&rest args) ""
                                    (message "sw %s %s %s %s" (window-minibuffer-p) (current-buffer) (selected-window) (evil-print-state))))
;(add-hook 'buffer-list-update-hook (lambda (&rest args) "" (message "Update %d" (setq scnt (1+ scnt)))))
(dolist (cus evil-custom-mode-line-alist)
  (add-hook (intern (format "evil-%s-state-entry-hook" (car cus)))
            (lambda nil ""
              (message "ei %s %s %s %s %s %s"
                       (window-minibuffer-p)
                       (current-buffer)
                       (selected-window)
                       (evil-print-state)
                       (window-buffer)
                       (window-mode-line-height)
                       )
              (or (or (not (eq (current-buffer) (window-buffer))) (window-minibuffer-p))
                  (funcall (intern (code-evil-mode-line-setter-name (car cus)))))
              )))
;(add-hook 'evil-motion-state-exit-hook (lambda nil "" (message "eo")))
(dolist (cus evil-custom-mode-line-alist)
  (add-hook (intern (format "evil-%s-state-exit-hook" (car cus)))
            (lambda nil ""
              (message "eo %s %s %s %s %s"
                       (window-minibuffer-p)
                       (current-buffer)
                       (selected-window)
                       (evil-print-state)
                       (window-buffer)
                       )
              ))
  )

(provide 'evil-lib)
; evil-lib.el ends here