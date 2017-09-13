;; -*- lexical-binding : t byte-compile-dynamic : t -*-

(require 'util-lib)
(eval-when-compile
  (require 'miscellany-code))

(defun window-mru ()
  "Move the cursor to previous buffer in other window."
  (interactive)
  (catch 'tag-break
    (dolist (buf (buffer-list (selected-frame)))
      (let* ((win (get-buffer-window buf)))
	(unless (or (not win)
		    (eq buf (current-buffer))
		    (eq win (selected-window)))
	  (select-window win)
	  (throw 'tag-break nil))))))

(defun next-non-system-buffer ()
  ""
  )

;; (code-add-advice
;;  (
;;   ivy-switch-buffer
;;   ;; select-window
;;   )
;;  :after
;;  (lambda (&rest args)
;;    (message "buffer: %s" (buffer-list))))

(code-define-temporary-minor-mode
 switch-window
 "Switch window minor mode. Press 'M-3' to cycle through active window list."
 `((,(kbd "M-3") . other-window)))

(code-define-temporary-minor-mode
 switch-buffer
 "Switch buffer minor mode. Press 'C-q' to cycle through buffer lists."
 `((,(kbd "C-q") . next-buffer)))

(code-defcmd-double-events
 super-close-window
 "1 - 'quit-other-window' 2 - 'delete-other-windows'"
 ((quit-other-window))
 ((delete-other-windows)))

(code-defcmd-double-events
 super-split-window
 "1 - 'split-window-right' 2 - 'split-window-below'"
 ((split-window-right))
 ((split-window-below)))

(code-defcmd-double-events
 super-switch-window
 ""
 ((window-mru))
 ((other-window 1)
  (switch-window-mode 1)))

(code-defcmd-double-events
  super-switch-buffer
  ""
  ((mode-line-other-buffer))
  ((next-buffer)
   (switch-buffer-mode 1)))

(defun set-overriding-local-map (keymap)
  "Set keymap to 'overriding-local-map'. If 'overriding-local-map' is not
nil, make a composed keymap to replace it."
  (if overriding-local-map
      (unless (eq overriding-local-map keymap)
	(unless overriding-local-map-list
	  (push overriding-local-map overriding-local-map-list))
	(when (and overriding-local-map-list
		   (memq keymap overriding-local-map-list))
	  (setq overriding-local-map-list
		(delq keymap overriding-local-map-list)))
	(push keymap overriding-local-map-list)
	(setq overriding-local-map
	      (make-composed-keymap overriding-local-map-list)))
    (setq overriding-local-map keymap)))

(defun recover-overriding-local-map (keymap)
  "Clear keymap from 'overriding-local-map' even when it it 
a composed keymap."
  (when overriding-local-map
    (if (eq overriding-local-map keymap)
	(setq overriding-local-map nil)
      (when (memq keymap overriding-local-map-list)
	(setq overriding-local-map-list
	      (delq keymap overriding-local-map-list))
	(if (eq (length overriding-local-map-list) 1)
	    (setq overriding-local-map (car overriding-local-map-list)
		  overriding-local-map-list nil)
	  (setq overriding-local-map
		(make-composed-keymap overriding-local-map-list)))))))

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

;;; {{ X selection code for tty terminal
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
;;; }}

(provide 'miscellany-lib)
; miscellany.el ends here
