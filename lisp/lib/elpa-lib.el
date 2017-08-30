;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

(require 'util-lib)

(defconst pkg-exist nil "Package exists.")
(defconst pkg-miss nil "Package misses.")
(defconst install-succ nil "Install success.")
(defconst install-fail nil "Install fail.")
(defconst delete-succ nil "Delete success.")
(defconst delete-fail nil "Delete fail.")
(defconst upgrade-succ nil "Upgrade success.")
(defconst upgrade-fail nil "Upgrade fail.")
(defconst up-to-date nil "Package is up to date.")

(defsubst get-package-desc (pkg cl)
  "Get package-desc of symbol PKG from content list CL.

Return:
package-desc - if PKG is available in CL.
nil - otherwise."
  (let* ((pc (assq pkg cl)))
    (if pc (cadr pc) nil)))

(defun local-package-desc (pkg)
  "Get local package-desc of symbol PKG.

Return:
package-desc - if PKG is available in CL.
nil - otherwise."
  (get-package-desc pkg package-alist))

(defun remote-package-desc (pkg)
  "Get remove package-desc of symbol PKG.

Return:
package-desc - if PKG is available in CL.
nil - otherwise."
  (get-package-desc pkg package-archive-contents))

(defun package-out-of-date-p-raw (desc-local desc-remote)
  "Check whether the given package is out of date.

You need to make sure DESC-LOCAL and DESC-REMOTE are generated
by the given package and are available.
Return:
non-nil - out of date.
nil - up to date."
  (let* ((ver-local (package-desc-version desc-local))
        (ver-remote (package-desc-version desc-remote)))
    (version-list-< ver-local ver-remote)))

(defun elpa-install-raw (pkg)
  "Install symbol PKG using ELPA.

No matter whether PKG is installed or available in remote.
You need to check by yourself for safety."
  ;; (with-no-message (package-install pkg))
  (package-install pkg))

(defun elpa-delete-raw (pkg-desc)
  "Delete package determined by package descriptor PKG-DESC using ELPA.

No matter whether PKG is installed or available in local.
You need to check by yourself for safety."
  (package-delete pkg-desc))

(defun package-out-of-date-p (pkg)
  "Check whether symbol PKG is out of date.

You need to make sure that symbol PKG is available in both local
and remote content list (package-alist & package-archive-contents).
Return:
non-nil - out of date.
nil - up to date."
  (let* ((desc-local (local-package-desc pkg))
        (desc-remote (remote-package-desc pkg)))
    (package-out-of-date-p-raw desc-local desc-remote)))

(defun elpa-install (pkg)
  "Install symbol PKG using ELPA.

Return:
'pkg-exist, 'install-succ, 'install-fail"
  (if (package-installed-p pkg)
      'pkg-exist
    (if (remote-package-desc pkg)
        (progn (elpa-install-raw pkg) 'install-succ)
      'install-fail)))

(defun elpa-delete (pkg)
  "Delete symbol PKG using ELPA.

Return:
'pkg-miss, 'delete-succ, 'delete-fail(Not usual case. Need check.)"
  (if (package-installed-p pkg)
      (let* ((pkg-desc (local-package-desc pkg)))
        (if pkg-desc
            (progn (elpa-delete-raw pkg-desc) 'delete-succ)
          'delete-fail))
    'pkg-miss))

(defun elpa-upgrade (pkg)
  "Upgrade symbol PKG.

Return:
'upgrade-fail(Usually miss package), 'upgrade-succ, 'up-to-date"
  (let* ((desc-local nil)
         (desc-remote nil))
    (cond
     ((not (setq desc-local (local-package-desc pkg))) 'upgrade-fail)
     ((not (setq desc-remote (remote-package-desc pkg))) 'upgrade-fail)
     (t
      (if (package-out-of-date-p-raw desc-local desc-remote)
          (progn
            (elpa-delete-raw desc-local)
            (elpa-install-raw pkg)
            'upgrade-succ)
        'up-to-date)))))

(defun compute-dependency-family ()
  "Construct necessary package list by dependency."
  (let* ((rlt nil)
        (dep nil)
        (dep-last nil)
        (reqs nil)
        (desc nil))
    (dolist (pkg elpa-custom-packages-list)
      (when (package-installed-p pkg)
        (add-to-list 'rlt pkg)))
    (setq dep-last rlt)
    (while dep-last
      (setq dep nil)
      (dolist (pkg dep-last)
        (when (setq desc (remote-package-desc pkg))
          (when (setq reqs (package-desc-reqs desc))
            (dolist (rq reqs)
              (add-to-list 'dep (car rq))))))
      (setq dep-last nil)
      (when dep
        (dolist (pkg dep)
          (unless (memq pkg rlt)
            (add-to-list 'rlt pkg)
            (add-to-list 'dep-last pkg)))))
    rlt))

(defun install-package ()
  "Install packages listed in packages-list."
  (package-refresh-contents)
  ;; (or package-archive-contents (package-refresh-contents))
  (let* ((rlt-alist nil)
         (delete-list nil)
         (dep nil)
         (rlt nil))
    ;; Install packages in packages-list. Upgrade it if exists.
    (dolist (pkg elpa-custom-packages-list)
      (add-to-list 'rlt-alist
                   (cons pkg
                         (if (eq (setq rlt (elpa-install pkg)) 'pkg-exist)
                             (progn
                               (elpa-upgrade pkg))
                           rlt))))
    ;; Delete packages which is not tracked or dependented by
    ;; packages-list.
    (setq dep (compute-dependency-family))
    (dolist (pa package-alist)
      (unless (memq (car pa) dep)
        (add-to-list 'delete-list (car pa))))
    (when delete-list
      (dolist (dl delete-list)
        (add-to-list 'rlt-alist (cons dl (elpa-delete dl)))))
                                        ; Print result summary.
    (when rlt-alist
      (dolist (pkg rlt-alist)
        (message "[%s] %s" (symbol-name (cdr pkg)) (symbol-name (car pkg)))))))

(provide 'elpa-lib)
; elpa-lib.el ends here