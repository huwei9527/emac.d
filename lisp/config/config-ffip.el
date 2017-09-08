;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

(eval-when-compile
  (require 'code))

(require 'ffip-custom)

(code-defkey-ctl-x
 "f" find-file-in-project-by-selected)

;; Set the 'ffip-project-root' dynamically
;; Get around the error if 'ffip-project-root' can't find
;; a valid directory.
(code-add-advice (ffip-project-root)
                 :around
                 (lambda (orig-fun &rest args)
                   (let* ((rlt
                           (condition-case err
                               (apply orig-fun args)
                             (wrong-type-argument
                              (let* ((sym-err (car err))
                                     (data-err (cdr err)))
                                (unless (equal data-err '(stringp nil))
                                  (signal sym-err data-err)))))))
                     (unless rlt
		       (if (file-equal-p default-directory "~")
			   (setq rlt ffip-custom-default-directory)
			 (setq rlt default-directory)))
                     rlt)))

(provide 'config-ffip)
;; config-ffip.el ends here
