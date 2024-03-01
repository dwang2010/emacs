;; ------------------------------------------------------------------------
;; python configs
;; LSP: https://github.com/python-lsp/python-lsp-server
;; ------------------------------------------------------------------------
;; configure location of python interpreter (system dependent)
(if (eql system-type 'darwin)
    (setq-default python-shell-interpreter "/opt/homebrew/bin/python3.10")
  (setq-default python-shell-interpreter "/usr/bin/python3"))

;; single indent for multi-line function signature
(setq-default python-indent-def-block-scale 1)

;; silence warnings around indent guess
(setq-default python-indent-guess-indent-offset-verbose nil)

;; ------------------------------------------------------------------------
;; golang configs
;; ensure PATH / GOPATH properly set externally!
;; LSP: https://github.com/golang/tools/blob/master/gopls/doc/emacs.md
;; ------------------------------------------------------------------------
(use-package go-mode
  :ensure t
  :hook (go-ts-mode . my-go-cfg-hook)
  :config
  ;; goimports acts as superior replacement for gofmt
  ;; https://pkg.go.dev/golang.org/x/tools/cmd/goimports
  (setq-default gofmt-command "goimports")
  ;; fix dumb default indent settings
  (setq-default go-ts-mode-indent-offset 4))

(defun my-go-cfg-hook ()
  ;; hack: ensure go-mode load when opening in go-ts-mode
  (gofmt-before-save)
  ;; hack: gofmt-before-save hard-coded to only work on go-mode
  (add-hook 'before-save-hook 'gofmt nil t))

;; org babel support for golang
(use-package ob-go :ensure t)

;; invoke gofmt after editing org src block via (C-c ')
(define-advice org-edit-src-exit (:before (&rest _args))
  (when (memq major-mode '(go-mode go-ts-mode)) (gofmt)))

;; env related workaround
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
