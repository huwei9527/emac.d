;;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

;;; Commentary:

;;; Code:

(eval-when-compile (/require-meta core hook keymap))

(/require-custom company)
(/require-lib company)

(/eval-after-load company
  (/def-keys company-active-map
    TAB      company-complete-selection
    [tab]    company-complete-selection
    RET      company-newline
    [return] company-newline
    )
  (/def-keys company-filter-map
    TAB      company-complete-selection
    [tab]    company-complete-selection
    RET      company-newline
    [return] company-newline
    )
  ;; company statistic
  (setq company-statistics-file
	(expand-file-name "company-statistics-cache.el"
			  /custom-config-company-directory)
	company-require-match nil)
  (/add-hook (company-mode-hook) company-statistics-mode)
  (/advice-add-silence company-manual-begin)
  )

(/provide)
;;; config/company.el ends here
