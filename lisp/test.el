;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

(defun incc ()
  (setq abcd (1+ abcd))
  (print abcd))

(setq bb nil)

(defun testa (&optional a)
  "Test function a."
  (interactive)
  (message "%s %s %s %s %s %s %s"
           (propertize "ABCD" 'face '(:foreground "#e80000"))
           (propertize "ABCD" 'face '(:foreground "#444488"))
           (propertize "ABCD" 'face '(:foreground "#006fa0"))
           (propertize "ABCD" 'face '(:foreground "red"))
           (propertize "ABCD" 'face '(:foreground "white"))
           (propertize "ABCD" 'face '(:foreground "green"))
           (propertize "ABCD" 'face '(:foreground "blue"))
           ))

(setq str (propertize "SHow" 'face '(:foreground "red")))
(let ((fn (expand-file-name "test_compile.el" user-emacs-directory))
      (fnb (expand-file-name "test_compile.elc" user-emacs-directory)))
  ;(byte-compile-file (expand-file-name "test_compile.el" user-emacs-directory))
  (load fn)
  )
(switch-to-buffer "*Messages*")
(provide 'test)
; test.el ends here