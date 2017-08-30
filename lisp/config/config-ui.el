;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

(require 'ui-custom)

;; Hide GUI.
(and (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(and (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(and (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(setq inhibit-startup-screen t
      inhibit-startup-echo-area-message nil)

;;; Scroll up & down line continuously, not by half page.
(setq-default scroll-up-aggressively 0.0
              scroll-down-aggressively 0.0)

; maximize GUI window (frame for Emacs).
(add-to-list 'default-frame-alist `(fullscreen . maximized))
(add-to-list 'initial-frame-alist `(background-mode . dark))
(add-to-list 'default-frame-alist `(background-mode . dark))

;; Line number.
(global-linum-mode)
(column-number-mode 1)
(setq linum-delay t)
; Don't show line number in some certain major mode.
(advice-add 'linum-on :around
            (lambda (func &rest args)
              (or (memq major-mode ui-custom-inhibit-linum-mode-list)
                (apply func args))))

(require 'config-theme)

;;; Simple mode-line.
;; Don't show minor modes infomation which is the 5-th element.
(delete (nth 4 mode-line-modes) mode-line-modes)

;; Show overwrite mode with enable
(defvar mode-line-overwrite-mode
  `(overwrite-mode
    ,(propertize "+"
                 'face 'red-black
                 'help-echo "Overwrite mode.")
    ,(propertize ">"
                 'help-echo "Insert mode."))
  "Show overwrite mode status in mode line.")
(put 'mode-line-overwrite-mode 'risky-local-variable t)

(setq mode-line-frame-identification
      `(:eval (if (or (null window-system)
                      (eq window-system 'pc))
                  "%F"
                "")))

;; Color line number and column
(defvar mode-line-line-column
  (let* ((num-color 'blue-background))
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
mouse-1: Display Line and Column Mode Menu")))))))))
(setcar (nthcdr 2 mode-line-position) mode-line-line-column)

;; Show vc-mode status.
(defvar mode-line-vc-mode
  `(vc-mode
    (:eval (format "(%s)" (substring vc-mode 1)))))
(put 'mode-line-vc-mode 'risky-local-variable t)

;; Color tag if using meacs client.
(setq mode-line-client
      `(:propertize
        (:eval
         (if (frame-parameter nil 'client)
             (propertize "@" 'face 'blue-background)
           ""))
        help-echo ,(purecopy "emacsclient frame")))

;; Color remote file tag
(setq-default mode-line-remote
              `(:propertize
                (:eval (if (file-remote-p default-directory)
                           (propertize "%1@" 'face 'magenta-background)
                         "%1@"))
                mouse-face mode-line-highlight
                help-echo ,(purecopy
                            (lambda (window _object _point)
                              (format "%s"
                                      (if (stringp default-directory)
                                          (concat
                                           (if (file-remote-p default-directory)
                                               "Current directory is remote: "
                                             "Current directory is local: ")
                                           default-directory)
                                        "Current directory is nil"))))))

;; Color read-only and modified tag.
(setq-default mode-line-modified
              `(""
                (:propertize
                 (:eval (if buffer-read-only
                            (propertize "%1*" 'face 'magenta-background)
                          "%1*"))
                 help-echo mode-line-read-only-help-echo
                 local-map ,(purecopy (make-mode-line-mouse-map
                                       'mouse-1
                                       #'mode-line-toggle-read-only))
                 mouse-face mode-line-highlight)
                (:propertize
                 (:eval (if (buffer-modified-p)
                            (propertize "%1+" 'face 'magenta-background)
                          "%1+"))
                 help-echo mode-line-modified-help-echo
                 local-map ,(purecopy (make-mode-line-mouse-map
                                       'mouse-1
                                       #'mode-line-toggle-modified))
                 mouse-face mode-line-highlight)))

(setq-default mode-line-format
              (list
               ;; memory full message if it happens.
               "%e"
               ;; starting character "-" or " "
               'mode-line-front-space
               ;; Overwrite mode indicator
               'mode-line-overwrite-mode
               ;; Input method state and coding system.
               'mode-line-mule-info
               ;; %% - state of read only and modification
               'mode-line-modified
               ;; Frame info.
               'mode-line-frame-identification
               ;; Emacs client frame indicator
               'mode-line-client
               ;; file remote indicator
               'mode-line-remote
               ;; Buffer name. 12 characters.
               'mode-line-buffer-identification
               ;; major modes and narrow infomation
               " " 'mode-line-modes
               ;; Version control state.
               'mode-line-vc-mode
               ;; column, percentage, row
               "[" 'mode-line-position "]"
               ;; which-func and global-mode-string
               'mode-line-misc-info
               ;; ending character "-"
               'mode-line-end-spaces
               ))

(provide 'config-ui)
; config-ui.el ends here