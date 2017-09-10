;;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

(eval-when-compile
  (require 'code))

(require 'company-lib)

;;; Use TAB to select completion and RET insert newline
(code-eval-after-load
 company
 (code-define-key
   company-active-map nil
   "TAB" company-complete-selection
   "RET" company-newline)
 (code-define-key
   company-filter-map nil
   "TAB" company-complete-selection
   "RET" company-newline))

(provide 'config-company)
;;; config-company.el ends here
