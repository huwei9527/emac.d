;;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

;;; Commentary:

;;; Code:

(/require-lib format)
(/require-meta format)

(when t
  (let* ((i 0) j s)
    (message "     0     1     2     3     4     5     6     7     8     9     10")
    (while (< i 10)
      (setq j 0 s `(,(format "%s: " i)))
      (while (< j 12)
	(push (format "\033[%sm AbC \033[0m " (+ i (* j 10))) s)
	(setq j (1+ j)))
      (message (apply #'concat (nreverse s)))
      (setq i (1+ i)))))

(when nil
  (message "\033[1;3;20m ABCD \033[0m")
  (message "%s" (/--format-csi-parameter 1 2 3))
  (message "%s" (/--format-sgr 1 2 3))
  (message "reset: %s" /format-sgr-reset)
  (message "reset: %s" (/format-sgr-reset)))

(when t
  (message "s: %s" (/format-sgr "ABCD"))
  (message "s: %s" (/format-sgr "ABCD" :reset))
  ;(message "s: %s" (/format-sgr "ABCD" :reset-off))
  (message "s: %s" (/format-sgr "ABCD" :cyan :white-bg :italic :bold :faint
				:crossed :underline :blink :reverse))
  (message "s: %s" (/format-sgr "ABCD" :256-200-bg))
  (message "s: %s" (/format-sgr "ABCD" :true-100-50-20-bg))
  ;(/ppmacroexpand (/def-format-color-functions))
  (message "s: %s" (/format-red "ABCD"))
  (message "s: %s" (/format-cyan "ABCD"))
  )

(/provide)
;;; test/format.el ends here
