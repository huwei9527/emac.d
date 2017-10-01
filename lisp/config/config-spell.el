;;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

(eval-when-compile
  (require 'code)
  (require 'hook-code))

(code-eval-after-load
 flyspell
 (flyspell-lazy-mode 1)
 (add-to-list 'ispell-extra-args "--sug-mode=ultra")
 ;; (setq flyspell-large-region 1)
 )

(setq flyspell-issue-message-flag nil)
(code-add-hook
 (prog-mode-hook)
 (lambda ()
   (unless (scratch-buffer-p)
     (set (make-local-variable 'ispell-extra-args)
	  '("--sug-mode=ultra"
	    "--run-together"
	    "--run-together-limit=16"
	    "--run-together-min=2"))
     (flyspell-prog-mode))))
(code-add-hook
 (text-mode-hook)
 flyspell-mode)


(provide 'config-spell)
;;; config-spell.el ends here
