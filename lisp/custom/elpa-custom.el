;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

(eval-when-compile (require 'code-gen))

(defconst elpa-custom-packages-list
  '(
    evil
    ivy
    color-theme-solarized
    swiper
    )
  "Package to be install.")

(code-gen-defdir "packages"
  "The directory to store ELPA packages. This will be set to
package-user-dir automatically.")

(code-gen-defdir-config "package-describes"
  "The directory to save ELPA describe file. When you describe a
package (C-m or <RET> in the package mode) which is no installed,
ELPA will download a package-name-readme.txt file to describe the
package to you. The file is store in the package-user-dir directory
by default which would dirty the directory. So I hack it to this
directory by advice describe-package-1.")


(provide 'elpa-custom)
; elpa-custom.el ends here