;; -*- lexical-binding : t byte-compile-dynamic : t -*-

(require 'file-custom)
(require 'sequence-lib)

(defun scratch-buffer-p (&optional buf)
  "Detect whether BUF is *scratch* buffer."
  (string= (buffer-name buf) "*scratch*"))

(defun scroll-other-window-up-one-line ()
  "Scroll up other window by one line."
  (interactive)
  (scroll-other-window 1))

(defun scroll-other-window-down-one-line ()
  "Scroll down other window by one line."
  (interactive)
  (scroll-other-window -1))

(defun shell-command-stdin (cmd stdin &optional buf buf-error)
  "Execute CMD with STDIN as input."
  (shell-command
   (format "echo %s | %s" (shell-quote-argument stdin) cmd)
   buf buf-error))

(defun shell-command-to-string-stdin (cmd stdin)
  "Execute CMD with STDIN as input, collect and return result as string."
  (shell-command-to-string
   (format "echo %s | %s" (shell-quote-argument stdin) cmd)))

(defun wait-for-event (&optional n secs)
  "Wait for N (default 1) events for maximum SECS seconds."
  (or n (setq n 1))
  (let* ((max-time (when secs
		     (time-add (current-time)
			       (seconds-to-time secs))))
	 rlt)
    (catch 'tag-break
      (dotimes (i n)
	(push (read-event nil nil secs) rlt)
	(when (and max-time
		   (time-less-p max-time (current-time)))
	  (throw 'tag-break nil))))
    (vconcat (nreverse rlt))))

(defun put-back-event (&optional events force)
  "Put events back to 'unread-command-events'"
  (or events (setq events (this-command-keys-vector)))
  (setq events (collect-non-nil-element events))
  (when force
    (setq events (mapcar (lambda (c) (cons t c)) events)))
  (setq unread-command-events
	(append unread-command-events events)))

;;; {{ Character predictor
(defsubst char-space-p (c)
  "Return t if C is a space character."
  (eq ?\s (char-syntax c)))

(defsubst char-word-p (c)
  "Return t if C is a word constituent."
  (eq ?w (char-syntax c)))

(defsubst char-path-delimiter-p (c)
  "Return t if C is a path delimiter"
  (or (eq c ?\/) (eq c ?\\)))

(defsubst char-escape-p (c)
  "Return t if C is a escape character"
  (eq c ?\\))

(defsubst char-not-path-p (c)
  "Return t if C is not a path constituent."
  (or (and (<= 0 c) (<= c 32))
      (memq c file-custom-invalid-path-char)))

(defsubst char-at-point-word-p ()
  "Return t if the charactor before point is space or point is
at the begging of buffer."
  (and (not (eq (point) (line-beginning-position)))
       (let* ((c (preceding-char)))
	 (or (char-word-p c)
	     (eq ?\$ c)
	     (eq ?\- c)
	     (eq ?\@ c)))))
;;; }}

(defun path-at-point (&optional buffer start-position)
  "Return the (beg path end) position of the path if find."
  (interactive)
  (let* (pos-start pos-beg pos-mid pos-end)
    (save-excursion
      (when buffer (set-buffer buffer))
      (setq pos-start (if start-position start-position (point)))
      ;; search backward
      (let* ((pos-curr pos-start)
             (ch-curr (char-before pos-curr)))
        (when ch-curr
          (setq pos-end pos-curr)
          ;; Handle the first character specially
          ;; because it may be path delimiter character
          (when (and (char-path-delimiter-p ch-curr))
            (let* ((ch-fol (char-after pos-curr)))
              (unless (and ch-fol
                           (char-not-path-p ch-fol)
                           (char-escape-p ch-curr))
                (setq pos-mid pos-curr))))
          (let* ((pos-limit (if (> (- pos-start (point-min))
                                   file-custom-path-max)
                                (- pos-start file-custom-path-max)
                              (point-min)))
                 ch-pre path-p escape-p delimeter-p)
            (catch 'tag-break
              (while t
                (setq ch-pre (char-before (1- pos-curr)))
                (setq path-p (not (char-not-path-p ch-curr))
                      escape-p (char-escape-p ch-pre))
                (if (or path-p escape-p)
                    (progn
                      (when (and delimeter-p
                                 (char-path-delimiter-p ch-curr))
                        (unless pos-mid (setq pos-mid pos-curr)))
                      (setq delimeter-p path-p))
                  (setq pos-beg pos-curr)
                  (throw 'tag-break nil))
                (setq pos-curr (1- pos-curr))
                (if (> pos-curr pos-limit)
                    (setq ch-curr ch-pre)
                  (setq pos-beg pos-curr)
                  (throw 'tag-break nil)))))))
      ;; search forward
      (let* ((pos-curr pos-start)
             (ch-curr (char-after pos-curr)))
        (when ch-curr
          (unless pos-beg (setq pos-beg pos-curr))
          ;; Handle the first character specially
          ;; because it may be path delimiter character
          (when (char-path-delimiter-p ch-curr)
            (let* ((ch-fol (char-after (1+ pos-curr))))
              (unless (and ch-fol
                           (char-not-path-p ch-fol)
                           (char-escape-p ch-curr))
                (setq pos-mid (1+ pos-curr)))))
          (let* ((pos-limit (if (> (- (point-max) pos-start)
                                   file-custom-path-max)
                                (+ pos-start file-custom-path-max)
                              (point-max)))
                 (ch-pre (char-before pos-curr))
                 path-p escape-p delimeter-p)
            (catch 'tag-break
              (while t
                (setq path-p (not (char-not-path-p ch-curr))
                      escape-p (char-escape-p ch-pre))
                (if (or path-p escape-p)
                    (progn
                      (when (and delimeter-p
                                 (char-path-delimiter-p ch-curr))
                        (setq pos-mid (1+ pos-curr)))
                      (setq delimeter-p path-p))
                  (setq pos-end pos-curr)
                  (throw 'tag-break nil))
                (setq pos-curr (1+ pos-curr))
                (if (< pos-curr pos-limit)
                    (setq ch-pre ch-curr
                          ch-curr (char-after pos-curr))
                  (setq pos-end pos-curr)
                  (throw 'tag-break nil))))))))
    (list pos-beg pos-mid pos-end)))

(provide 'util-lib)
; utility.el ends here
