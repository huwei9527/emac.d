;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

(eval-when-compile
  (require 'test-code)
  (require 'hook-code)
  (require 'evil-code))

(require 'test-custom)
;; (require 'sequence-test)

(defun test-message-sibling ()
  ""
  (let* ((ch (window-child (window-parent))))
    (while ch
      (message "%s" ch)
      (setq ch (window-next-sibling ch)))))

(defun test-message-window-parameter ()
  ""
  (message "%s" (window-parameters)))

(defun test-message (&rest args)
  ""
  (interactive "P")
  ;; (message "args %s" args)
  (message "test-message")
  (save-selected-window
    ;; (other-window 1)
    ;; (require 'config-yasnippet)
    ;; (message "%s\np: %s\nn: %s"
    ;; 	     (selected-window)
    ;; 	     (window-prev-buffers)
    ;; 	     (window-next-buffers))
    ;; (test-message-sibling)
    ;; (test-message-window-parameter)
    ))

(code-defkey-ctl-c
 "C-t" test-message)


(defmacro code-trace-function (funs &optional before after)
  "Trace function."
  (code-progn
   (dolist (fun funs)
     (code-item
      `(code-add-advice (,fun) :before
			(lambda (&rest args)
			  (message "Before %s" ,(symbol-name fun))
			  ,(when before before)))
      `(code-add-advice (,fun) :after
			(lambda (&rest args)
			  (message "After %s" ,(symbol-name fun))
			  ,(when after after)))))))
(code-record-macro code-trace-function)

(defmacro display-in-help-window (buf-name &rest arg-forms)
  "Show content in help buffer BUF-NAME."
  `(with-help-window ,buf-name
     (with-current-buffer ,buf-name
       ,@arg-forms)))

(defun current-buffer-state-string ()
  "The string of current buffer state."
  (format "%-12s %-12s %-12s" (current-buffer) (window-buffer) (selected-window)))

(defun test-trace-message-function (tag)
  "The function to print the state of current buffer."
  (lambda (&rest args)
    (with-temp-message
        (format "%s: %s"
                tag
                (current-buffer-state-string)))))

(defmacro code-deftrace-advice (fun tag &optional wh &rest args)
  "Trace function using advice"
  (let* ((fun-name (symbol-name fun)))
    `(defun ,(intern-format "trace-%s" fun-name) ()
       ,(format "Trace '%s'" fun-name)
       (code-add-advice (,fun) ,(if wh wh :after)
                        ,(test-trace-message-function tag)))))

(defmacro code-deftrace-hook (hk tag &rest args)
  "Trace hook"
  (let* ((hk-name (symbol-name hk)))
    `(defun ,(intern-format "trace-%s" hk-name) ()
       ,(format "Trace '%s'" hk-name)
       (code-add-hook (,hk) ,(test-trace-message-function tag)))))

(defmacro code-deftrace-hook-for-evil-hook (type)
  "Trace evil entry hook."
  (code-progn
   (dolist (cus code-evil-custom-state-config)
     (let* ((st (car cus))
            (st-name (symbol-name st)))
       (code-item
        `(code-deftrace-hook
          ,(intern-format "evil-%s-state-%s-hook" st-name type)
          ,(format "e%cse%c" (aref st-name 0) (aref type 1))))))))

(defmacro code-set-trace-for-evil-hook (type)
  "Trace evil hook."
  (code-progn
   (dolist (cus code-evil-custom-state-config)
     (let* ((st (car cus)))
       (code-item
        `(,(intern-format "trace-evil-%s-state-%s-hook"
                          (symbol-name st) type)))))))

(code-deftrace-advice switch-to-buffer "stb__")
(code-deftrace-advice select-window "sw___")
(code-deftrace-advice quit-window "qw___")

(code-deftrace-hook temp-buffer-window-setup-hook "tbwsp")
(code-deftrace-hook temp-buffer-window-show-hook "tbwsw")
(code-deftrace-hook first-change-hook "fc___")

(code-deftrace-hook-for-evil-hook "entry")
(code-deftrace-hook-for-evil-hook "exit")

(defun color-list-to-hex-raw (color-list)
  "Convert color list to hex string"
  (apply 'format "#%02x%02x%02x" color-list))

(defun color-list-to-hex (color-list)
  "Convert color list to hex string."
  (let* ((color-hex (color-list-to-hex-raw
                     (mapcar (lambda (c) (lsh c -8)) color-list))))
    (if (string= color-hex "#000000")
        (color-list-to-hex-raw color-list)
      color-hex)))

(defun color-to-hex (color-name)
  "Convert the color defined by COLOR-NAME to hex string."
  (if (color-defined-p color-name)
      (color-list-to-hex (color-values color-name))
    nil))

(defun hex-to-color-name (color-hex)
  "Get the color name defined by hex string."
  (let* ((color-name (get-defined-color-name color-hex)))
    (unless color-name (setq color-name color-hex))
    color-name))

(defvar sample-string "[Aa]" "Sample string.")
(defvar hex-to-color-name-hash-table nil
  "Hash table for looking up color name.")

(defun generate-hex-to-color-name-hash-table ()
  "Generate hash table for looking up color name."
  (unless hex-to-color-name-hash-table
    (setq hex-to-color-name-hash-table
          (make-hash-table :test 'equal :size 1000))
    (dolist (color-name (defined-colors))
      (puthash (color-to-hex color-name) color-name
               hex-to-color-name-hash-table)))
  hex-to-color-name-hash-table)

(defun get-defined-color-name (color-hex)
  "Get the defined color name."
  (unless hex-to-color-name-hash-table
    (generate-hex-to-color-name-hash-table))
  (gethash color-hex hex-to-color-name-hash-table))

(defun color-sample (color)
  "Show color"
  (propertize sample-string 'face `(:foreground ,color)))

(defun face-sample (face)
  "Show face."
  (format "%s %-12s %-12s %-12s %-12s [%s]"
          (propertize sample-string 'face face)
          (hex-to-color-name (face-attribute face :foreground nil t))
          (hex-to-color-name (face-attribute face :background nil t))
          (face-attribute face :width)
          (face-attribute face :weight)
          (symbol-name face)))

(defun face-list-sample (face-list)
  (let* ((sam-list nil))
    (dolist (face face-list)
      (push (face-sample face) sam-list)
      (push "\n" sam-list))
    (setq sam-list (cdr sam-list))
    (apply 'concat (nreverse sam-list))))

(defvar font-lock-face-list
  '(keyword warning function-name comment comment-delimiter
            type constant builtin preprocessor string
            doc negation-char variable-name))

(defvar basic-face-list
  '(default bold italic underline fixed-pitch
     variable-pitch shadow link link-visited
     highlight match error warning success))

(defun basic-faces-sample ()
  "Show basic faces."
  (face-list-sample basic-face-list))

(defun font-lock-faces-sample ()
  "Show font lock faces."
  (let* ((face-list
          (mapcar
           (lambda (c)
             (intern-format "font-lock-%s-face" (symbol-name c)))
           font-lock-face-list)))
    (face-list-sample face-list)))

(defun display-font-lock-face ()
  "Show font-lock face."
  (display-in-help-window "*ShowFace*"
   (insert (font-lock-faces-sample))
   (newline)
   (insert (basic-faces-sample))
   (newline)
   (require 'ui-code)
   (insert (face-list-sample code-basic-color-face-list))
   (newline)
   (insert (face-sample 'mode-line-highlight))
   ))

(provide 'test-lib)
;; test-lib.el ends here
