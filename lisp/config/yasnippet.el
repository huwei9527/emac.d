;;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

;;; Commentary:

;;; Code:

(eval-when-compile (/require-meta core))

(/require-custom yasnippet)

(/eval-after-load yasnippet
  (setq yas-snippet-dirs
	(remove yas--default-user-snippets-dir yas-snippet-dirs)
	yas--default-user-snippets-dir
	/custom-config-snippets-directory)
  (add-to-list 'yas-snippet-dirs /custom-config-snippets-directory))

(/provide)
;;; config/yasnippet.el ends here
