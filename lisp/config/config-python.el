;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

(eval-when-compile
  (require 'hook-code)
  (require 'keymap-code)
  (require 'silence-code))

(require 'python-lib)

(code-eval-after-load
 python
 (setq python-shell-interpreter "python3")
 (elpy-enable)
 (code-add-advice
  (python-indent-guess-indent-offset)
  :around
  (lambda (orig-fun &rest args)
    (code-silence
     (apply orig-fun args))))
 (code-add-hook
  (python-mode-hook)
  electric-pair-mode))

(code-eval-after-load
 elpy
 (setq elpy-rpc-python-command "python3")
 ;; (elpy-use-ipython)
 ;; Don't confirm kill process when exit emacs
 (code-add-advice
  (python-shell-make-comint)
  :filter-return
  (lambda (rlt)
    (set-process-query-on-exit-flag (get-buffer-process rlt) nil)
    rlt))
 (code-defkey-ctl-c-local
  elpy-mode-map
  "C-c" python-super-compile)
 ;; Use new interpreter process every time
 (code-add-advice
  (elpy-shell-send-region-or-buffer)
  :before
  (lambda (&rest args)
    (elpy-shell-kill))))

(provide 'config-python)
;;; config-python.el ends here
