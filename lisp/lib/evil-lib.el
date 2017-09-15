;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

(require 'evil-custom)
(eval-when-compile
  (require 'evil-code)
  (require 'code)
  (require 'miscellany-code))
(require 'util-lib)

(code-define-temporary-minor-mode
 scroll-other-window
 "Scroll buffer in other window."
 `((,(kbd "k") . scroll-other-window-down-one-line)
   (,(kbd "j") . scroll-other-window-up-one-line)
   (,(kbd "l") . scroll-other-window-down)
   (,(kbd "n") . beginning-of-buffer-other-window)
   (,(kbd "m") . end-of-buffer-other-window)
   (,(kbd "SPC") . scroll-other-window-with-space)))

(code-defcmd-double-events
  scroll-other-window-with-space
  "Use space key to scroll other window"
  ((scroll-other-window))
  ((scroll-other-window-mode -1)))



(provide 'evil-lib)
; evil-lib.el ends here
