;;; main emacs configuration

;; ------------------------------------------------------------------------
;; package management
;; ------------------------------------------------------------------------
(require 'package)
(require 'use-package) ;; native from v29.1
(setq-default use-package-verbose t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(defun my-ensure-package-version (package &optional version)
  "Install newest version or ensure installed version >= target version (as str)"
  (unless package--initialized (package-initialize t))
  (unless package-archive-contents (package-refresh-contents))
  (let* ((current (cadr (assoc package package-alist)))
         (current-version (if current (package-desc-version current) '(-1)))
         (pkg-desc (cadr (assoc package package-archive-contents)))
         (pkg-version (and pkg-desc (package-desc-version pkg-desc)))
         (target-version (or (and (stringp version) (version-to-list version))
                             pkg-version)))
    (when (version-list-< current-version target-version)
      (if (null pkg-desc)
          (error "Package `%s' not found in package archives" package))
      (if (version-list-< pkg-version target-version)
          (error "A suitable version of `%s' is not available" package))
      (package-install pkg-desc)
      (if current (package-delete current)))
    nil))

;; ------------------------------------------------------------------------
;; fix env related paths
;; ------------------------------------------------------------------------
(if (eql system-type 'darwin)
    (add-to-list 'exec-path "~/homebrew/bin/"))

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
