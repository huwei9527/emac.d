(require 'config-custom)

;; ELPA
(setq package-enable-at-startup nil)
(setq package-archives
      '(("gnu" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/") ;; Tsinghua mirror
        ("melpa" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))
; Default ELPA's user package directory.
(setq package-user-dir config-packages-directory)
(package-initialize)

(provide 'config-elpa)
; config-elpa.el ends here