;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

(require 'elpa-custom)

;; ELPA
(setq package-enable-at-startup nil)
(setq package-archives
       ;; Tsinghua mirror
      '(("gnu" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
        ("melpa" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))
; Default ELPA's user package directory.
(setq package-user-dir config-packages-directory)
(package-initialize)

;; When you describe-package or Press Enter or C-m in the packages list, you
;; will see a pop window with package description. If the package is not
;; installed, package.el will download a readme file and store in the
;; 'package-user-dir' which is annoying.  This advice put all the readme file in
;; some other directory to keep the 'package-user-dir' clean.
(advice-add 'describe-package-1 :around
            (lambda (func pkg)
              (let ((package-user-dir config-package-describes-directory))
                (apply func `(,pkg)))))

(provide 'config-elpa)
;; config-elpa.el ends here
