;;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

;;; Commentary:

;;; Code:

(eval-when-compile (/require-meta hook))
(/require-lib format)
(/require-custom elpa)

(defvar /--default-package-alist (if (boundp 'package-alist) package-alist)
  "Default package alist.")

(defun /--package-name (pkg)
  "Return the package name (a symbol).
If PKG is a symbol, return itself.
If PKG is a description structure, return the name of it.
If PKG is a string, return the interned symbol."
  (cond
   ((symbolp pkg) pkg)
   ((package-desc-p pkg) (package-desc-name pkg))
   ((stringp pkg) (intern pkg))
   (t (error "Invalid package %s" pkg))))

(defun /--package-desc (pkg &optional alist)
  "Return the package description structure.
ALIST is the alist storing the description structures of packages.
If ALIST is nil, default to `/--default-package-alist'"
  (or alist (setq alist /--default-package-alist))
  (cadr (assq (/--package-name pkg) alist)))

(defun /--package-dependence (pkg &optional alist)
  "Return the dependence list of package PKG in ALIST.
See `/--package-name' `/--package-desc' for more explanation."
  (package-desc-reqs (/--package-desc pkg alist)))

(defun /--package-version (pkg &optional alist)
  "Return the version for package PKG in ALIST.
See `/--package-name' `/--package-desc' for more explanation."
  (package-desc-version (/--package-desc pkg alist)))

(defun /package-remote-desc (pkg)
  "Return the description structure of PKG in the server.
See `/--package-name' and `package-archive-contents'."
  (/--package-desc pkg package-archive-contents))

(defun /package-desc (pkg)
  "Return the description structure of current using PKG.
See `/--package-name' and `package-alist'."
  (/--package-desc pkg package-alist))

(defun /package-remote-version (pkg)
  "Return the version of PKG in the server.
See `/--package-name' and `package-archive-contents'."
  (/--package-version pkg package-archive-contents))

(defun /package-version (pkg)
  "Return the version of current using PKG.
See `/--package-name' and `package-alist'."
  (/--package-version pkg package-alist))

(defun /package-remote-dependence (pkg)
  "Return the dependence of PKG in the server.
See `/--package-name' and `package-archive-contents'."
  (/--package-dependence pkg package-archive-contents))

(defun /package-dependence (pkg)
  "Return the dependence of current using PKG.
See `/--package-name' and `package-alist'."
  (/--package-dependence pkg package-alist))

(defun /--package-tag (tag user)
  "The colored tag of operation for packages."
  (if (eq user 'user) (format "[%s]*" tag) (format "[%s] " tag)))

(defun /--package-resolve-dependency ()
  "Return the dependence packages of the packages in `package-selected-packages'."
  (let* (deps new next dep)
    (setq new package-selected-packages)
    (while new
      (setq next nil)
      (dolist (pkg new)
	(dolist (attrs (/package-remote-dependence pkg))
	  (setq dep (car attrs))
	  (unless (package-built-in-p dep)
	    ;(if (eq dep 'auctex) (message "%s" pkg))
	    (unless (memq dep deps)
	      (push dep deps)
	      (push dep next)))))
      (setq new next))
    deps))

(defun /--package-collect-info (plist tag)
  "Colloct information about package in PLIST.
Devide them into three categories: new package (install), old package (upgrade),
  up to date package (uptodate). The return value is a list of three list in
  this category order."
  (let* (installs upgrades uptodates)
    (dolist (pkg plist)
      (let* ((rversion (/package-remote-version pkg)))
	(if (package-installed-p pkg)
	    (let* ((version (/package-version pkg)))
	      (if (version-list-< version rversion)
		  (push `(,pkg ,tag ,rversion ,version) upgrades)
		(push `(,pkg ,tag ,rversion) uptodates)))
	  (push `(,pkg ,tag ,rversion) installs))))
    (list installs upgrades uptodates)))

(defun /package-install ()
  "Install packages in `package-selected-packages'.
Also upgrade installed packages and remove packages no longer needed."
  (package-refresh-contents)
  (let* ((deps (/--package-resolve-dependency))   ; dependent packages
	 (dels (package--removable-packages))     ; package not needed
	 installs upgrades deletes uptodates
	 uinfo dinfo
	 )
    ;; remove duplicated dependent packages that is already in
    ;; `package-selected-packages'.
    (dolist (pkg package-selected-packages)
      (if (assq pkg package-archive-contents)
	  (setq deps (delq pkg deps))
	(error "Invalid package %s" pkg)))
    ;; user installed packages
    (let* ((uinfo (/--package-collect-info package-selected-packages 'user))
	   (dinfo (/--package-collect-info deps 'dep)))
      (setq installs (append (nreverse (car uinfo)) (nreverse (car dinfo)))
	    uinfo (cdr uinfo) dinfo (cdr dinfo)
	    upgrades (append (nreverse (car uinfo)) (nreverse (car dinfo)))
	    uinfo (cdr uinfo) dinfo (cdr dinfo)
	    uptodates (append (nreverse (car uinfo)) (nreverse (car dinfo)))
	    ))
    (dolist (pkg dels)
      (push `(,pkg ,(/package-version pkg)) deletes))
    ;; install all
    (let* ((package-load-list '(all)))
      (package-install-selected-packages))
    ;; upgrade all
    (dolist (attrs upgrades)
      (package-reinstall (car attrs)))
    ;; remove uneccesary package
    (package-autoremove)
    (dolist (attrs installs)
      (let* ((pkg (car attrs)) (attrs (cdr attrs))
	     (tag (car attrs)) (attrs (cdr attrs))
	     (rversion (car attrs)))
	(message "%s %s %s"
		 (/--package-tag (/format-yellow "C") tag)
		 (/format-yellow (symbol-name pkg)) rversion)))
    (dolist (attrs upgrades)
      (let* ((pkg (car attrs)) (attrs (cdr attrs))
	     (tag (car attrs)) (attrs (cdr attrs))
	     (rversion (car attrs)) (attrs (cdr attrs))
	     (version (car attrs)))
	(message "%s %s %s <= %s"
		 (/--package-tag (/format-green "U") tag)
		 (/format-green (symbol-name pkg)) rversion version)))
    (dolist (attrs deletes)
      (let* ((pkg (car attrs)) (attrs (cdr attrs))
	     (version (car attrs)))
	(message "%s %s %s"
		 (/--package-tag (/format-red "D") nil)
		 (/format-red (symbol-name pkg)) version)))
    ;; (dolist (attrs uptodates)
    ;;   (let* ((pkg (car attrs)) (attrs (cdr attrs))
    ;; 	     (tag (car attrs)) (attrs (cdr attrs))
    ;; 	     (version (car attrs)))
    ;; 	(message "%s %s %s"
    ;; 		 (/--package-tag "E" tag) pkg version)))
    ))

(/provide)
;;; lib/elpa.el ends here
