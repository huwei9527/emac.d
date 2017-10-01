;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-


(eval-when-compile
  (require 'code)
  (require 'hook-code)
  (require 'keymap-code))

(code-eval-after-load
 tex
 (setq TeX-auto-save t
       TeX-parse-self t
       ;; TeX-electric-math (cons "$" "$")
       ;; LaTeX-electric-left-right-brace t
       ;; TeX-electric-sub-and-superscript t
       Tex-fold-auto t
       TeX-fold-preserve-comments t
       TeX-debug-bad-boxes t
       TeX-debug-warnings t
       TeX-error-overview-open-after-TeX-run t
       TeX-clean-confirm nil
       preview-auto-cache-preamble t
       reftex-plug-into-AUCTeX t)
 (code-add-hook
  (TeX-mode-hook)
  LaTeX-math-mode
  TeX-fold-mode
  ;; prettify-symbols-mode
  electric-pair-mode
  ;; TeX-source-correlate-mode
  reftex-mode
  (lambda ()
    ;; (code-define-key LaTeX-mode-map nil "$" self-insert-command)
    (company-mode-on)
    ;; (setq TeX-raise-frame-function #'x-focus-frame)
    (set (make-local-variable 'company-backends)
	 '((company-auctex-macros company-auctex-symbols
				  company-auctex-environments)
	   company-auctex-bibs
	   company-auctex-labels
	   (company-dabbrev company-keywords))))))

(code-eval-after-load
 evil
 (code-eval-after-load
  tex-buf
  (evil-set-initial-state 'TeX-output-mode 'motion)
  (evil-make-overriding-map TeX-output-mode-map 'motion)
  (evil-set-initial-state 'TeX-error-overview-mode 'motion)
  (evil-make-overriding-map TeX-error-overview-mode-map 'motion)
  ))


(provide 'config-tex)
;;; config-tex.el ends here
