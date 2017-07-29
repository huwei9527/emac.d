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

(defun testaa (a)
  "Test function aa"
  (testa a))

(defun testaaa (a)
  "Test function aaa"
  (testaa a)
  (defun testinaaa ()
    (interactive)
    (message "abcd")))
(message "Test Eager Out.")
(defvar fuck-var "FUCK B")
;(setq fuck-var "FUCK B")
(defmacro testmacro () ""
          (let ((a nil))
            (message "Test Eager In.")
            (print fuck-var)
            (setq a fuck-var)
            `(progn
               ,(setq a fuck-var)
               ,(print fuck-var)
               (message ,a))))

(print fuck-var)
(testmacro)

(setq str (propertize "SHow" 'face '(:foreground "red")))
;(message str)
(message "Compile start")
(let ((fn (expand-file-name "test_compile.el" user-emacs-directory))
      (fnb (expand-file-name "test_compile.elc" user-emacs-directory)))
  (eval (eval-when-compile (list 'message "Return of eval-when-compile")))
  ;(byte-compile-file (expand-file-name "test_compile.el" user-emacs-directory))
  (load fn)
  )
(message "Compile end.")
(switch-to-buffer "*Messages*")
(provide 'test)
; test.el ends here