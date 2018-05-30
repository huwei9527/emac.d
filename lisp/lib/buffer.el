;;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

;;; Commentary:

;;; Code:

(eval-when-compile (/require-meta core keymap))

(/require-custom buffer)
(/require-lib file core)

;;; {{ error predicate
(defun /error-read-only-p (data context caller)
  "Return t if error is read-only type."
  (let* ((type (car data)))
    (or (eq type 'text-read-only)
	(eq type 'buffer-read-only))))

(defun /error-buffer-boundary-p (data context caller)
  "Return t if error is buffer boundary type."
  (let* ((type (car data)))
    (or (eq type 'beginning-of-buffer)
	(eq type 'end-of-buffer)
	(eq type 'beginning-of-line)
	(eq type 'end-of-line))))

(defun /error-history-p (data context caller)
  "Return t if error is history type."
  (and (eq (car data) 'user-error)
       (let* ((mess (cadr data)))
	 (or (string-prefix-p "Beginning of history" mess)
	     (string-prefix-p "End of history" mess)))))
;;; }}

(defun /window-mru ()
  "Select most recently used window.
Intend to jump between two recently used window."
  (interactive)
  (catch 'break-tag
    (let* (win)
      (dolist (buf (buffer-list (selected-frame)))
	(setq win (get-buffer-window buf))
	(unless (or (not win)                  ; visible window
		    (eq buf (current-buffer))  ; not same buffer
		    (eq win (selected-window)) ; not same window
		    )
	  (select-window win)
	  (throw 'break-tag nil))))))

(defun /next-window ()
  "Switch to next live window in cyclic order."
  (interactive)
  (other-window 1))

(defun /previous-window ()
  "Switch to previous live window in cyclic order."
  (interactive)
  (other-window -1))

(defun /--buffer-ignore-p ()
  "Return non-nil if current buffer is a less significant buffer.
See `/custom-buffer-major-mode-ignore-list' and `/system-buffer-p'."
  (or (memq major-mode /custom-buffer-major-mode-ignore-list)
      (and (buffer-name) (/system-buffer-p (buffer-name)))))

(defun /next-buffer ()
  "Select `next-buffer', try to ignore some type of buffer."
  (interactive)
  (let* ((old (current-buffer)))
    (next-buffer)
    (or (eq old (current-buffer))
	(catch 'break-tag
	  (while (/--buffer-ignore-p)
	    (next-buffer)
	    (and (eq old (current-buffer))
		 (throw 'break-tag nil)))))))

(defun /previous-buffer ()
  "Select `previous-buffer', try to ignore some type of buffer."
  (interactive)
  (let* ((old (current-buffer)))
    (previous-buffer)
    (or (eq old (current-buffer))
	(catch 'break-tag
	  (while (/--buffer-ignore-p)
	    (previous-buffer)
	    (and (eq old (current-buffer))
		 (throw 'break-tag nil)))))))

(defun /buffer-live-in-other-window-p (&optional window)
  "Return non-nil if buffer in WINDOW is also displayed in another
  live window.
If WINDOW is nil, choose the selected window."
  (or window (setq window (selected window)))
  (and (window-live-p window)
       (let* ((buf (window-buffer window)) rlt)
	 (and buf (catch 'break-tag
		    (dolist (win (window-list-1))
		      (when (and (/neq win window)
				 (eq buf (window-buffer win)))
			(setq rlt t)
			(throw 'break-tag t)))))
	 rlt)))

(defun /temporary-buffer-p (&optional buf)
  "Return non-nil if BUF is temporary buffer."
  (or buf (setq buf (current-buffer)))
  (with-current-buffer buf
    (or (memq major-mode /custom-temporary-buffer-major-mode-list)
	(/temporary-buffer-prefix-p (buffer-name)))))

(defun /kill-temporary-buffer (win pred)
  "Kill buffer in window WIN with predicate PRED."
  (let* (kill prev)
    (while (and (window-valid-p win)
		(window-live-p win)
		(window-prev-buffers win)
		(funcall pred (window-buffer win)))
      (quit-window 'kill win)
      (setq kill t))
    (and (window-valid-p win)
	 (window-live-p win)
	 (or (window-prev-buffers win) (delete-window win)))
    kill))

(defun /close-other-buffer ()
  "Close buffer in the other window.
If the corresponding buffer is temporary buffer, kill it."
  (interactive)
  (or (one-window-p)
      (let* ((win (next-window)) (first t) (closed t) buf)
	(while closed
	  (setq closed nil)
	  ;; Invoke buffer local close-buffer-function.
	  (and /custom-close-other-buffer-function
	       (if (funcall /custom-close-other-buffer-function)
		   (setq closed t first nil)))
	  ;; Delete global temporary buffer.
	  (if (/kill-temporary-buffer win #'/temporary-buffer-p)
	      (setq closed t first nil))
	  ;; If no buffer is deleted, delete the one normal buffer.
	  (when first
	    (setq first nil closed t)
	    (quit-window nil win))))))

(defun /scroll-other-window-line-down (&optional count)
  "Scroll the other window COUNT lines downwards."
  (interactive)
  (or count (setq count -1))
  (scroll-other-window count))

(defun /scroll-other-window-line-up (&optional count)
  "Scroll the other window COUNT lines upwards."
  (interactive)
  (or count (setq count 1))
  (scroll-other-window count))

;;; {{ other window
(/def-transient-minor-mode other-window
  "Cycle other window.
Press `2' or `3' to cycle backward or forward."
  `(([?3] . /next-window)
    ([?2] . /previous-window))
  "other-window")

(/def-double-keys-event-command other-window
  ((/window-mru))
  ((other-window 1)
   (call-interactively '/other-window-transient-mode))
  "Cycle through live windows.\nSee `/other-window-transient-mode'.")
;;; }}

;;; {{ other-buffer
(/def-transient-minor-mode other-buffer
  "Cycle buffer in current window.
Use `q' and `Q' to cycle forward and backward.
Use `tab' to cycle backward."
  `(([?q] . /next-buffer)
    ([?Q] . /previous-buffer)
    ([?\t]. /previous-buffer))
  "other-buffer")

(/def-double-keys-event-command other-buffer
  ((mode-line-other-buffer))
  ((/next-buffer)
   (call-interactively '/other-buffer-transient-mode))
  "Cycle through buffers in one window.
Normal: Switch between `mru' buffer in the selected window.
        See `mode-line-other-buffer'
Double: Cycle through buffers in the selected window. 
        See `/other-buffer-transient-mode'.")
;;; }}

(/def-double-keys-event-command close-other-window
  ((/close-other-buffer))
  ((delete-other-windows))
  "Close buffer in the other window.
Normal: Close buffers in other window. See `/close-other-buffer'.
Double: Delete all other windows. See `delete-other-window'.")

(/def-double-keys-event-command split-window
  ((save-selected-window (ivy-switch-buffer-other-window)))
  ((split-window-right))
  "Split window right.
Normal: Select a buffer and split window right and open that buffer.
        See `ivy-switch-buffer-other-window'.
Double: Split window right. See `split-window-right'.")


(/provide)
;;; lib/buffer.el ends here
