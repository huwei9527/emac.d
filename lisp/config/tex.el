;;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

;;; Commentary:

;;; Code:

(eval-when-compile (/require-meta core hook keymap evil company))

(/require-lib tex)

(/eval-after-load tex
  (setq Tex-auto-save t
	Tex-parse-self t
	;Tex-electric-math (cons "$" "$")
	;Latex-electric-left-right-brace t
	;Tex-electric-sub-superscript t
	Tex-fold-auto t
	Tex-fold-preserve-comments t
	Tex-debug-bad-boxes t
	Tex-debug-warnings t
	Tex-error-overview-open-after-TeX-run t
	Tex-clean-confirm nil
	preview-auto-cache-preamble t
	reftex-plug-into-AUCTeX t
	)
  (/add-hook (TeX-mode-hook)
    (lambda ()
      (LaTeX-math-mode 1)
      (TeX-fold-mode 1)
      ;(prettify-symbols-mode 1)
      (electric-pair-mode 1)
      ;(TeX-source-correlate-mode 1)
      (reftex-mode 1)
      (/company-mode-on (company-auctex-macros
			 company-auctex-symbols
			 company-auctex-environments)
			company-auctex-bibs
			company-auctex-labels
			(company-dabbrev company-keywords))
      (setq /custom-close-other-buffer-function #'/tex-close-other-buffer
	    ;TeX-raise-frame-function #'x-focus-frame
	    ))))

(/eval-after-load tex-buf
  ;; (/add-hook (TeX-after-compilation-finished-functions)
  ;;   /tex-close-window-after-compilation-finished)
  ;; (/advice-add (TeX-command-run-all TeX-command-buffer TeX-command-master)
  ;;   :before
  ;;   /tex-close-window-after-tex-command-advice)
  (require 'evil)
  (/evil-set-initial-state-and-make-override TeX-output-mode motion)
  (/evil-set-initial-state-and-make-override TeX-error-overview-mode motion))

(/provide)
;;; config/tex.el ends here
