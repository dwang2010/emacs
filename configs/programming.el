;; ------------------------------------------------------------------------
;; python configs
;; ------------------------------------------------------------------------
;; configure location of python interpreter (system dependent)
(if (eql system-type 'darwin)
    (setq-default python-shell-interpreter "/opt/homebrew/bin/python3.10")
  (setq-default python-shell-interpreter "/usr/bin/python3"))

;; single indent for multi-line function signature
(setq-default python-indent-def-block-scale 1)

;; ------------------------------------------------------------------------
;; golang configs
;; ------------------------------------------------------------------------
;(if (eql system-type 'darwin) (add-hook 'before-save-hook 'gofmt-before-save))

; env related workaround
;; (if (eql system-type 'darwin)
;;     (progn
;;       (setenv "PATH" (concat (getenv "PATH") ":/opt/homebrew/bin/"))
;;       (setq-default exec-path (append exec-path '("/opt/homebrew/bin/")))))

;; ------------------------------------------------------------------------
;; js-mode configs
;; ------------------------------------------------------------------------
(setq-default js-indent-level 4)

;; ------------------------------------------------------------------------
;; protobuf configs
;; ------------------------------------------------------------------------
; 4-space indent
(defconst my-protobuf-style
  '((c-basic-offset . 4)
    (indent-tabs-mode . nil)))

(add-hook 'protobuf-mode-hook
          (lambda () (c-add-style "my-style" my-protobuf-style t)))

;; ------------------------------------------------------------------------
;; slime - Superior Lisp Interaction Mode for Emacs
;; assumes prior installation of common lisp compiler (here sbcl)
;; ------------------------------------------------------------------------
(use-package slime
  :ensure t
  :config
  (setq-default inferior-lisp-program "sbcl"))

;; ------------------------------------------------------------------------
;; devdocs.io lookup
;; https://github.com/blahgeek/emacs-devdocs-browser
;; ------------------------------------------------------------------------
;; devdocs-browser-install-docs = get docs
;; devdocs-browser-open-in      = open docs in selected language
;; C-c C-o = opens current page in external browser
(use-package devdocs-browser
  :ensure t
  :bind (("C-h D" . devdocs-browser-open-in)))
