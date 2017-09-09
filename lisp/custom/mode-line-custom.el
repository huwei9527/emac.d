;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-


(defface mode-line-client-tag
  '((t (:inherit blue-foreground)))
  "The face for emacs client tag in mode line.")

(defface mode-line-remote-tag
  '((t (:inherit magenta-foreground)))
  "The face for remote file tag in mode line.")

(defface mode-line-modified-tag
  '((t (:inherit mode-line-remote-tag)))
  "The face for modified tag in mode line.")

(defface mode-line-read-only-tag
  '((t (:inherit mode-line-modified-tag)))
  "The face for read-only tag in mode line.")

(defface mode-line-line-column-number
  '((t (:inherit blue-foreground)))
  "The face for line number and column number in mode line.")

(defface mode-line-overwrite-tag
  '((t (:inherit evil-custom-insert-state-tag)))
  "The face for overwrite tag in mode line")

(defvar mode-line-overwrite-mode
  `(overwrite-mode
    ,(propertize "+"
                 'face 'mode-line-overwrite-tag
                 'help-echo "Overwrite mode.")
    ,(propertize ">"
                 'help-echo "Insert mode."))
  "Show overwrite mode status in mode line.")
(put 'mode-line-overwrite-mode 'risky-local-variable t)

(defvar mode-line-line-column
  (let* ((num-color 'mode-line-line-column-number))
    `(line-number-mode
      ((column-number-mode
        (10 (" "
             (:eval
              (propertize
               (format "(%s,%s)"
                       (propertize "%l" 'face ',num-color)
                       (propertize "%c" 'face ',num-color))
               'local-map mode-line-column-line-number-mode-map
               'mouse-face 'mode-line-highlight
               'help-echo "Line number and Column number \n\
mouse-1: Display Line and Column Mode Menu"))))
        (6 (" "
            (:eval
             (propertize
              (format "L%s"
                      (propertize "%l" 'face ',num-color))
              'local-map mode-line-column-line-number-mode-map
              'mouse-face 'mode-line-highlight
              'help-echo "Line number\n\
mouse-1: Display Line and Column Mode Menu"))))))
      ((column-number-mode
        (5 (" "
            (:eval
             (propertize
              (format "C%s"
                      (propertize "%c" 'face ',num-color))
              'local-map mode-line-column-line-number-mode-map
              'mouse-face 'mode-line-highlight
              'help-echo "Column number\n\
mouse-1: Display Line and Column Mode Menu"))))))))
  "Coloed line number and column number in mode line.")

(defvar mode-line-vc-mode
  `(vc-mode
    (:eval (format "(%s)" (substring vc-mode 1))))
  "VC mode in mode line.")
(put 'mode-line-vc-mode 'risky-local-variable t)


(provide 'mode-line-custom)
;; mode-line-custom.el ends here.
