;;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

;;; Commentary:

;;; Code:

(eval-when-compile (/require-meta hook keymap))
(/require-lib python)

(/eval-after-load python
  (setq python-shell-interpreter "python3")
  (elpy-enable)
  (/advice-add-silence python-indent-guess-indent-offset)
  (/add-hook (python-mode-hook) electric-pair-mode))

(/eval-after-load elpy
  (setq elpy-rpc-python-command "python3")
  ;(elpy-use-ipython)
  ;; Auto-kill python process when emacs exits.
  (/advice-add (python-shell-make-comint) :filter-return
    (lambda (rlt)
      (set-process-query-on-exit-flag (get-buffer-process rlt) nil)
      rlt))
  (/def-keys-ctl-c-mode elpy-mode C-c /python-compile)
  (/advice-add (elpy-shell-send-region-or-buffer) :before
    (lambda (&rest args) (elpy-shell-kill))))

(/provide)
;;; config/python.el ends here
