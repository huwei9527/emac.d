;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

(eval-when-compile
  (require 'code))

(defmacro code-load-solarized-theme ()
  "Load solarized theme."
  (code-progn
   (code-item
    `(set-frame-parameter
      nil 'background-mode color-theme-custom-solarized-style)
    `(set-terminal-parameter
      nil 'background-mode color-theme-custom-solarized-style)
    `(load-theme 'solarized t))))

(defvar code-color-alist
  ;;            256          t
  '((white    "color-255" "#ffffff")
    (black    "color-16"  "#000000")
    (red      "color-196" "#ff0000")
    (green    "color-46"  "#00ff00")
    (blue     "color-21"  "#0000ff")
    (yellow   "color-226" "#ffff00")
    (magenta  "color-201" "#ff00ff")
    (cyan     "color-51"  "#00ffff")
    ))

(defvar code-basic-color-face-list nil)

(defmacro code-defface-foreground-or-background-raw (cr-list fg-bg)
  "Define face contain single color for FG-BG."
  (let* ((cr (nth 0 cr-list))
         (cr-name (symbol-name cr))
         (cr-tty (nth 1 cr-list))
         (cr-hex (nth 2 cr-list))
         (sym-fb (intern-format ":%s" fg-bg))
         (sym-face (intern-format "%s-%s" cr-name fg-bg)))
    (push sym-face code-basic-color-face-list)
    `(defface ,sym-face
       '((((class color) (type tty) (min-colors 256)) ,sym-fb ,cr-tty)
         (t ,sym-fb ,cr-hex))
       ,(format "%s-%s face." cr-name fg-bg))))

(defmacro code-defface-foreground (cr-list)
  ""
  `(code-defface-foreground-or-background-raw ,cr-list "foreground"))

(defmacro code-defface-background (cr-list)
  ""
  `(code-defface-foreground-or-background-raw ,cr-list "background"))

(defmacro code-defface-foreground-and-background-raw (fg-list bg-list)
  (let* ((fg (car fg-list))
         (bg (car bg-list))
         (fg-name (symbol-name fg))
         (bg-name (symbol-name bg))
         (sym-face (intern-format "%s-%s" fg-name bg-name)))
    (push sym-face code-basic-color-face-list)
    `(defface ,sym-face
       '((t :inherit (,(intern-format "%s-foreground" fg-name)
                      ,(intern-format "%s-background" bg-name))))
       ,(format "%s-%s face." (capitalize fg-name) (capitalize bg-name)))))

(defmacro code-defface-basic-color ()
  "Define face with basic colors."
  (let* ((wb-alist (list (car code-color-alist)
                         (cadr code-color-alist)))
         (cr-alist (nthcdr 2 code-color-alist)))
    (code-progn
     (dolist (list-cr code-color-alist)
       (code-item `(code-defface-foreground ,list-cr)
                  `(code-defface-background ,list-cr)))
     (code-item `(code-defface-foreground-and-background-raw
                  ,(car wb-alist) ,(cadr wb-alist))
                `(code-defface-foreground-and-background-raw
                  ,(cadr wb-alist) ,(car wb-alist)))
     (dolist (list-wb wb-alist)
       (dolist (list-cr cr-alist)
         (code-item `(code-defface-foreground-and-background-raw
                      ,list-cr ,list-wb)
                    `(code-defface-foreground-and-background-raw
                      ,list-wb ,list-cr)))))))

(provide 'ui-code)
;; ui-code.el ends here
