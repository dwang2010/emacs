(require 'package)

;; add melpa repo for packages
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; ------------------------------------------------------------------------
;; use package configs
;; ------------------------------------------------------------------------
;; make sure use-package is installed
(unless (package-installed-p 'use-package) (package-install 'use-package))

(require 'use-package)
(setq-default use-package-verbose t)

;; ------------------------------------------------------------------------
;; update built-in-packages to target versions
;; ------------------------------------------------------------------------
(defun package-update (package &optional version)
  "Update a package from the package archives.
If VERSION is nil, update the package to the most recent version
available.  Otherwise, VERSION should be a version string, or a
list of the type returned by `version-to-list'. The package will
be updated only if the currently installed version is less than
the version specified, even if a newer version is available."
  (unless package--initialized
    (package-initialize t))
  (unless package-archive-contents
    (package-refresh-contents))
  (let* ((current (cadr (assoc package package-alist)))
         (current-version (if current (package-desc-version current) '(-1)))
         (pkg-desc (cadr (assoc package package-archive-contents)))
         (pkg-version (and pkg-desc (package-desc-version pkg-desc)))
         (target-version (or (and (stringp version) (version-to-list version))
                             version
                             pkg-version)))
    (when (version-list-< current-version target-version)
      (if (null pkg-desc)
        (error "Package `%s' not found in package archives" package))
      (if (version-list-< pkg-version target-version)
        (error "A suitable version of `%s' is not available" package))
      (package-install pkg-desc)
      (if current
          (package-delete current)))
    nil))

(package-update 'org '(9 7 4)) ;; ensure org 9.7.4

;; ------------------------------------------------------------------------
;; fix env related paths
;; ------------------------------------------------------------------------
(if (eql system-type 'darwin)
    (add-to-list 'exec-path "/opt/homebrew/bin/"))

;; ------------------------------------------------------------------------
;; load all the things
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Lisp-Libraries.html
;; ------------------------------------------------------------------------
(add-to-list 'load-path "~/.emacs.d/configs/")
(load-library "base")         ; basic settings + keybindings
(load-library "visual")       ; themes / display related
(load-library "programming")  ; language related default settings
(load-library "packs_native") ; native package configs
(load-library "packs_ext")    ; third-party package configs
(load-library "org_cfg")      ; org-mode related configs
