;;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

;;; Commentary:

;;; Code:

(eval-when-compile (/require-meta core hook))

(/require-custom spell)
(/require-lib file spell)

(/eval-after-load flyspell
  (flyspell-lazy-mode 1)
  (add-to-list 'ispell-extra-args "--sug-mode=ultra")
  (/add-hook (flyspell-mode-hook) /spell-set-personal-dictionary)
  (setq flyspell-large-region 1)
  )

;; (/advice-add (ispell-internal-change-dictionary
;; 	      ) :before
;;   (lambda (&rest args)
;;     (message "internal-change-dictionary: %s" args)
;;     ))

;; (/advice-add (ispell-start-process)
;;   (lambda (&rest args)
;;     (message "start-process: %s" args)))



(setq flyspell-issue-message-flag nil)
(setq ispell-personal-dictionary
      (expand-file-name /custom-spell-personal-default-file
			/custom-config-spell-directory))

(/add-hook (prog-mode-hook)
  (lambda (&rest args)
    (unless (/scratch-buffer-p)
      (set (make-local-variable 'ispell-extra-args)
	   '("--sug-mode=ultra"
	     "--run-together"
	     "--run-together-limit=16"
	     "--run-together-min=2"))
      (flyspell-prog-mode))))
(/add-hook (text-mode-hook) flyspell-mode)

(/provide)
;;; config/spell.el ends here
