;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

(eval-when-compile
  (require 'code))

;; (require 'find-file-in-project)

(code-defkey-ctl-x
 "f" find-file-in-project-by-selected)

;; Set the 'ffip-project-root' dynamically
;; Get around the error if 'ffip-project-root' can't find
;; a valid directory.
(code-add-advice (ffip-project-root)
                 :around
                 (lambda (orig-fun &rest args)
                   (let* ((rlt
                           (condition-case nil
                               (apply orig-fun args)
                             (error nil))))
                     (unless rlt (setq rlt default-directory))
                     rlt)))

(provide 'config-ffip)
;; config-ffip.el ends here