(setq emacs-load-start-time (current-time))
(defconst config-home-directory
  (file-name-as-directory (expand-file-name "~/Projects/emacs.d")))
(add-to-list 'load-path (file-name-as-directory (expand-file-name "lisp" config-home-directory)))

(require 'config-custom)
(require 'config-elpa)
(require 'config-miscellany)
(require 'config-ui)
(require 'config-auto-save)
(require 'config-slime)

(defvar emacs-load-time (time-to-seconds (time-since emacs-load-start-time)))
(message "%f" emacs-load-time)
(setq initial-scratch-message (format ";; %f\n" emacs-load-time))
