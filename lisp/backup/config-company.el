;;; config-company.el --- company configuration -*- lexical-binding : t ; byte-compile-dynamic : t -*-

;;; Code:

(eval-when-compile
  (require 'hook-code)
  (require 'keymap-code))


;;; Commentary:
;; 

(require 'company-custom)
(require 'company-lib)

;;; Use TAB to select completion and RET insert newline
(code-eval-after-load
 company
 (code-define-key
   company-active-map nil
   "TAB" company-complete-selection
   "RET" company-newline
   [tab] company-complete-selection
   [return] company-newline)
 (code-define-key
   company-filter-map nil
   "TAB" company-complete-selection
   "RET" company-newline
   [tab] company-complete-selection
   [return] company-newline)
 ;; company statistic
 (setq company-statistics-file
       (expand-file-name "company-statistics-cache.el"
			 config-company-directory))
 (code-add-hook
  (company-mode-hook)
  company-statistics-mode)
 (setq company-require-match nil))



(provide 'config-company)
;;; config-company.el ends here
