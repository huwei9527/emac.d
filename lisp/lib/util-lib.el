;; -*- lexical-binding : t byte-compile-dynamic : t -*-

(require 'file-custom)

(defun scroll-other-window-up ()
  "Scroll other window up"
  (interactive)
  (scroll-other-window '-))

(defun quit-other-window ()
  "Quit buffer in other window."
  (interactive)
  (if (> (length (window-list-1)) 1)
      (let* ((win-curr (selected-window))
             (win-other (next-window)))
        (select-window win-other)
        (quit-window 'kill)
        (select-window win-curr))))

(defun find-file-alternatively ()
  "Find file using alternative method.")

(defun next-non-system-buffer ()
  "Cycle through buffer whose name doesn't start with star '*'"
  (interactive)
  nil)

(defun shell-command-stdin (cmd stdin &optional buf buf-error)
  "Execute CMD with STDIN as input."
  (shell-command
   (format "echo %s | %s" (shell-quote-argument stdin) cmd)
   buf buf-error))

(defun shell-command-to-string-stdin (cmd stdin)
  "Execute CMD with STDIN as input, collect and return result as string."
  (shell-command-to-string
   (format "echo %s | %s" (shell-quote-argument stdin) cmd)))

(defun sequence-equal (seq1 seq2)
  "Return t if all the corresponding elements of SEQ1 and SEQ2 are equal."
  (let* ((seq1-p (sequencep seq1))
	 (seq2-p (sequencep seq2)))
    (cond
     ((eq seq1 seq2) t)
     ((and seq1-p seq2-p)
      (let* ((len (length seq1)) rlt)
	(if (eq len (length seq2))
	    (progn
	      (setq rlt t)
	      (catch 'tag-break
		(dotimes (i len)
		  (unless (sequence-equal (elt seq1 i) (elt seq2 i))
		    (setq rlt nil)
		    (throw 'tag-break nil))))))
	rlt))
     ((not (or seq1-p seq2-p))
      (equal seq1 seq2))
     (t nil))))

(defun wait-for-event (&optional n secs)
  "Wait for N (default 1) events for maximum SECS seconds."
  (or n (setq n 1))
  (let* (rlt)
    (dotimes (i n)
      (push (read-event nil nil secs) rlt))
    (vconcat (nreverse rlt))))

(defun put-back-event (events &optional force)
  "Put events back to 'unread-command-events'"
  (when events
    (setq events (collect-non-nil-element events))
    (when force
      (setq events (mapcar (lambda (c) (cons t c)) events)))
    (setq unread-command-events
	  (append unread-command-events events))))

(defun collect-non-nil-element (seq)
  "Return the list of the non-nil element in sequence SEQ."
  (when (and seq (sequencep seq))
    (let* ((rlt nil)
	   (len (length seq))
	   el-curr)
      (dotimes (i len)
	(when (setq el-curr (elt seq i))
	  (push el-curr rlt)))
      (nreverse rlt))))

;;; {{ Excute form with no output.
;;     TODO : Can't shadow file saving message ("Write ...")
(defun message-null (&rest args) "Dummy function." (ignore))

(defun message-mute ()
  "Mute message function."
  (advice-add 'message :override #'message-null)
  (advice-add 'minibuffer-message :override #'message-null)
  (advice-add 'message-box :override #'message-null)
  (advice-add 'message-or-box :override #'message-null)
  (advice-add 'display-message-or-buffer :override #'message-null)
  (advice-add 'print :override #'message-null)
  (advice-add 'prin1 :override #'message-null)
  (advice-add 'princ :override #'message-null)
  (advice-add 'pp :override #'message-null))

(defun message-unmute ()
  "Unmute message function."
  (advice-remove 'message #'message-null)
  (advice-remove 'minibuffer-message #'message-null)
  (advice-remove 'mesage-box #'message-null)
  (advice-remove 'message-or-box #'message-null)
  (advice-remove 'display-message-or-buffer #'message-null)
  (advice-remove 'print #'message-null)
  (advice-remove 'prin1 #'message-null)
  (advice-remove 'princ #'message-null)
  (advice-remove 'pp #'message-null))

(defmacro with-no-message (&rest form)
  "Run FORM with no message."
  `(progn
     (let* ((standard-output nil)
	    ;;(message-log-max nil)
	    )
       (message-mute)
       (unwind-protect
           (with-no-warnings
             ,@form)
         (message-unmute)))))
;; }}

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

(defun path-at-point (&optional buffer start-position)
  "Return the (beg path end) position of the path if find."
  (interactive)
  (let* ((pos-start nil)
         (pos-beg nil)
         (pos-mid nil)
         (pos-end nil))
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
    ;; (message "%s %s %s" pos-beg pos-mid pos-end)
    (list pos-beg pos-mid pos-end)))

(provide 'util-lib)
; utility.el ends here
