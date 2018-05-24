;;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

;;; Commentary:

;;; Code:

(eval-when-compile (/require-meta core hook))

(/require-custom spell)
(/require-lib file spell)

(/eval-after-load flyspell
  (flyspell-lazy-mode 1)
  (add-to-list 'ispell-extra-args "--sug-mode=ultra")
  (setq flyspell-large-region 1)
  (/add-hook (flyspell-mode-hook) /spell-set-personal-dictionary)
  ;; Inhibit saving message, Ispell process message
  (/advice-add (ispell-init-process ispell-kill-ispell ispell-pdict-save)
    :around
    (lambda (fun &rest args)
      "Silent functions."
      (/with-no-message (apply fun args))))
  ;; Don't ask to save personal dictionary. Save directly.
  (/advice-add (ispell-pdict-save) :around
    (lambda (fun &rest args)
      "Inhibit `y-or-n-p'."
      (/with-yes (apply fun args))))
  )

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
