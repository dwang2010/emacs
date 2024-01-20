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
