;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-


(eval-when-compile
  (require 'code))

(code-eval-after-load
 tex
 (setq TeX-auto-save t
       TeX-parse-self t
       TeX-electric-math (cons "$" "$")
       ;; LaTeX-electric-left-right-brace t
       ;; TeX-electric-sub-and-superscript t
       Tex-fold-auto t
       TeX-fold-preserve-comments)
 (code-add-hook
  (TeX-mode-hook)
  LaTeX-math-mode
  TeX-fold-mode
  prettify-symbols-mode
  (lambda ()
    (company-mode-on)
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
  (evil-make-overriding-map TeX-error-overview-mode-map 'motion)))


(provide 'config-tex)
;;; config-tex.el ends here
