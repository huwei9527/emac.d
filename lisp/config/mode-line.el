;;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

;;; Commentary:

;;; Code:

(/require-custom mode-line)

;; Don't show minor modes infomation which is the 5-th element.
(delete (nth 4 mode-line-modes) mode-line-modes)

(setq mode-line-frame-identification
      `(:eval
	(if (or (null window-system) (eq window-system 'pc)) "%F" "")))

;; Colored line and column number.
(setcar (nthcdr 2 mode-line-position) /mode-line-line-column)

;; Colored emacs client tag.
(setq mode-line-client
      `(:propertize
        (:eval
         (if (frame-parameter nil 'client)
             (propertize "@" 'face '/mode-line-client-tag)
           ""))
        help-echo ,(purecopy "emacsclient frame")))

;; Colored remote file tag.
(setq-default mode-line-remote
              `(:propertize
                (:eval (if (file-remote-p default-directory)
                           (propertize "%1@" 'face '/mode-line-remote-tag)
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

;; Colored read-only and modified tag.
(setq-default mode-line-modified
              `(""
                (:propertize
                 (:eval (if buffer-read-only
                            (propertize "%1*" 'face '/mode-line-read-only-tag)
                          "%1*"))
                 help-echo mode-line-read-only-help-echo
                 local-map ,(purecopy (make-mode-line-mouse-map
                                       'mouse-1
                                       #'mode-line-toggle-read-only))
                 mouse-face mode-line-highlight)
                (:propertize
                 (:eval (if (buffer-modified-p)
                            (propertize "%1+" 'face '/mode-line-modified-tag)
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
               '/mode-line-overwrite-mode
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
               '/mode-line-vc-mode
               ;; column, percentage, row
               "[" 'mode-line-position "]"
               ;; which-func and global-mode-string
               'mode-line-misc-info
	       ;; temporary minor mode tag
	       '/mode-line-transient-minor-mode
               ;; ending character "-"
               'mode-line-end-spaces
               ))

(/provide)
;;; config/mode-line.el ends here
