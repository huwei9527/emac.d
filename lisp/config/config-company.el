;;; config-company.el --- company configuration -*- lexical-binding : t ; byte-compile-dynamic : t -*-

;;; Code:

(eval-when-compile
  (require 'code))


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
 (code-add-hook
  (company-mode-hook)
  company-statistics-mode)
 (setq company-require-match nil))

;; company statistic
(setq company-statistics-file
      (expand-file-name "company-statistics-cache.el"
			config-company-directory))


(provide 'config-company)
;;; config-company.el ends here
