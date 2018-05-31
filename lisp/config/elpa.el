;;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

;;; Commentary:

;;; Code:

(/require-custom elpa)

;; ELPA
(setq package-enable-at-startup nil
      ;; archives source
      package-archives /custom-package-archives
      ;; Default ELPA's user package directory.
      package-user-dir /custom-packages-directory
      ;; stop package to add (package-initialize) to your init.el file
      package--init-file-ensured t)

;; setup user instaled package and load selected packages
;; see /custom-package-alist
(setq package-selected-packages nil package-load-list nil)
(let* (pkg load)
  (dolist (attrs /custom-package-alist)
    (setq pkg (car attrs) load (cdr attrs))
    (push pkg package-selected-packages)
    (or (eq load t) (push `(,pkg ,load) package-load-list)))
  (push 'all package-load-list))

;; load user installed packges
(package-initialize)

;; When you describe-package or Press Enter or C-m in the packages
;; list, you will see a pop window with package description. If the
;; package is not installed, package.el will download a readme file
;; and store in the 'package-user-dir' which is annoying.  This advice
;; put all the readme file in some other directory to keep the
;; 'package-user-dir' clean.
(advice-add 'describe-package-1 :around
            (lambda (func pkg)
              (let ((package-user-dir
		     /custom-config-package-describes-directory))
                (apply func `(,pkg)))))

(/provide)
;;; config/elpa.el ends here
