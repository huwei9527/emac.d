;;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

;;; Commentary:

;;; Code:

(/require-lib core)
(/require-custom select)

(defun /term-set-selection (type data)
  "Set gui selection in terminal."
  (or type (setq type 'PRIMARY))
  (if (and data (stringp data))
      (pcase type
	('PRIMARY
	 (/shell-command-to-string
	  /custom-term-set-selection-primary-command data))
	('CLIPBOARD
	 (/shell-command-to-string
	  /custom-term-set-selection-clipboard-command data))
	(_ (signal 'error `("invalid type" ,type))))
    (signal 'error `("invalid data" ,data)))
  data)

(defun /term-selection-value (type)
  "Get gui selection in terminal."
  (let* (text)
    (or type (setq type 'PRIMARY))
    (pcase type
      ('PRIMARY
       (setq text (/shell-command-to-string
		   /custom-term-selection-value-primary-command)))
      ('CLIPBOARD
       (setq text (/shell-command-to-string
		   /custom-term-selection-value-clipboard-command)))
      (_ (signal 'error `("invalid type" ,type))))
    (and text
	 (remove-text-properties 0 (length text)
				 '(foreign-selection nil) text))
    text))

(defun /gui-select-text-advice (fun text)
  "Set gui selection in terminal.
In other graphic display, call original `gui-select-text'."
  (if (eq (framep (selected-frame)) t)
      (progn
	(when select-enable-primary
	  (/term-set-selection 'PRIMARY text)
	  (setq gui--last-selected-text-primary text))
	(when select-enable-clipboard
	  (setq saved-region-selection text)
	  (/term-set-selection 'CLIPBOARD text)
	  (setq gui--last-selected-text-clipboard text)))
    (funcall fun text)))

(defun /gui-selection-value-advice (fun)
  "Get gui selection in terminal.
In other graphic display, call original `gui-selection-value'."
  (if (eq (framep (selected-frame)) t)
      (let* (ptext ctext)
	(when select-enable-primary
	  (setq ptext (/term-selection-value 'PRIMARY)
		ptext
		(cond
		 ((and ptext (string= ptext ""))
		  (setq gui--last-selected-text-primary nil))
		 ((eq      ptext gui--last-selected-text-primary) nil)
		 ((string= ptext gui--last-selected-text-primary)
		  (setq gui--last-selected-text-primary ptext) nil)
		 (t (setq gui--last-selected-text-primary ptext)))))
	(when select-enable-clipboard
	  (setq ctext (/term-selection-value 'CLIPBOARD)
		ctext
		(cond
		 ((and ctext (string= ctext ""))
		  (setq gui--last-selected-text-clipboard nil))
		 ((eq      ctext gui--last-selected-text-clipboard) nil)
		 ((string= ctext gui--last-selected-text-clipboard)
		  (setq gui--last-selected-text-clipboard ctext) nil)
		 (t (setq gui--last-selected-text-clipboard ctext)))))
	(setq next-selection-coding-system nil)
	(or ctext ptext))
    (funcall fun)))

(/provide)
;;; lib/select.el ends here
