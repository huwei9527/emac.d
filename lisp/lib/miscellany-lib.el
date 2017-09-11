;; -*- lexical-binding : t byte-compile-dynamic : t -*-

(require 'util-lib)

(defun super-tab ()
  "Tab to do everything."
  (interactive)
  (let* ((point-last (point)))
    (unless (and (char-at-point-word-p)
		 (and (boundp 'company-mode) company-mode
		      (with-no-message
		       (call-interactively 'company-manual-begin))))
      (call-interactively 'indent-for-tab-command)
      (when (eq (point) point-last)
	(call-interactively 'toggle-hideshow-block)))))

(defun x-set-selection-shell (type data)
  "Set X selection with shell command."
  (or type (setq type 'PRIMARY))
  (if (and data (stringp data))
      (cond
       ((eq type 'PRIMARY)
	(shell-command-to-string-stdin
	 x-set-primary-selection-shell-command data))
       ((eq type 'CLIPBOARD)
	(shell-command-to-string-stdin
	 x-set-clipboard-selection-shell-command data))
       (t (signal 'error (list "invalid type" type))))
    (signal 'error (list "invalid data" data)))
  data)

(defun x-selection-value-shell (type)
  "Get X selection with shell command."
  (let* (text)
    (or type (setq type 'PRIMARY))
    (cond
     ((eq type 'PRIMARY)
      (setq text
	    (shell-command-to-string
	     x-primary-selection-value-shell-command)))
     ((eq type 'CLIPBOARD)
      (setq text
	    (shell-command-to-string
	     x-clipboard-selection-value-shell-command)))
     (t (signal 'error (list "invalid type" type))))
    (when text
      (remove-text-properties 0 (length text)
			      '(foreign-selection nil) text))
    text))

(defun x-select-text-around-advice (origin-fun text)
  "Set X selection in character-only terminal. In other terminal
call original 'x-select-text'."
  (if (eq (framep (selected-frame)) t)
      (progn
	(when x-select-enable-primary
	  (x-set-selection-shell 'PRIMARY text)
	  (setq x-last-selected-text-primary text))
	(when x-select-enable-clipboard
	  (setq saved-region-selection text)
	  (x-set-selection-shell 'CLIPBOARD text)
	  (setq x-last-selected-text-clipboard text)))
    (funcall origin-fun text)))

(defun x-selection-value-around-advice (origin-fun)
  "Get X selection in character-only terminal.

Return t to stop advice function invode 'x-selection-value'
Otherwise, return nil"
  (if (eq (framep (selected-frame)) t)
      (let* (clip-text primary-text)
	(when x-select-enable-clipboard
	  (setq clip-text (x-selection-value-shell 'CLIPBOARD))
	  (when (string= clip-text "") (setq clip-text nil))
	  (setq clip-text
		(cond
		 ((or (not clip-text) (string= clip-text ""))
		  (setq x-last-selected-text-clipboard nil))
		 ((eq      clip-text x-last-selected-text-clipboard) nil)
		 ((string= clip-text x-last-selected-text-clipboard)
		  (setq x-last-selected-text-clipboard clip-text) nil)
		 (t (setq x-last-selected-text-clipboard clip-text)))))
	(when x-select-enable-primary
	  (setq primary-text (x-selection-value-shell 'PRIMARY))
	  (setq primary-text
		(cond
		 ((or (not primary-text) (string= primary-text ""))
		  (setq x-last-selected-text-primary nil))
		 ((eq      primary-text x-last-selected-text-primary) nil)
		 ((string= primary-text x-last-selected-text-primary)
		  (setq x-last-selected-text-primary primary-text) nil)
		 (t (setq x-last-selected-text-primary primary-text)))))
	(setq next-selection-coding-system nil)
	(or clip-text primary-text))
    (funcall origin-fun)))

;; (message "aaa: %s bbb" (x-selection-value-shell 'PRIMARY))

(provide 'miscellany-lib)
; miscellany.el ends here
