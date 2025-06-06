;; ------------------------------------------------------------------------
;; python configs
;; LSP: https://github.com/python-lsp/python-lsp-server
;; ------------------------------------------------------------------------
;; configure location of python interpreter (system dependent)
;; (if (eql system-type 'darwin)
;;     (setq-default python-shell-interpreter "/opt/homebrew/bin/python3.10")
;;   (setq-default python-shell-interpreter "/usr/bin/python3"))
(setq-default python-shell-interpreter "/usr/bin/python3")

;; single indent for multi-line function signature
(setq-default python-indent-def-block-scale 1)

;; silence warnings around indent guess
(setq-default python-indent-guess-indent-offset-verbose nil)

;; ------------------------------------------------------------------------
;; rust configs
;; LSP: https://github.com/rust-lang/rust-analyzer
;; ------------------------------------------------------------------------
;; (use-package rust-mode :ensure t :defer t)
;; (use-package rustic :ensure t :after (rust-mode) :defer t)

;; ;; add rust related components to exec path
;; (add-to-list 'exec-path "~/.cargo/bin")

;; ;; use rust-ts-mode as major mode for correct treesitter font locking
;; (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-ts-mode))

;; (use-package rust-ts-mode
;;   :defer t
;;   :hook (rust-ts-mode . my-rust-cfg-hook))

;; (defun my-rust-cfg-hook ()
;;   (local-set-key (kbd "C-c C-p") #'rustic-popup)
;;   (add-hook 'before-save-hook 'rust-format-buffer nil t))

;; ------------------------------------------------------------------------
;; c++ configs
;; LSP: https://clangd.llvm.org/installation.html
;; ------------------------------------------------------------------------
;; (use-package c-ts-mode
;;   :defer t
;;   :preface
;;   (defvar my-cpp-func-sig-arg-indent 4
;;     "Number of spaces to indent for multi-line function signature arguments")
;;   ;; https://google.github.io/styleguide/cppguide.html#Function_Declarations_and_Definitions

;;   (defun my-cpp-indent-style()
;;     `(;; fix indent for multi-line function signature
;;       ((match nil "parameter_list" nil 1 1) standalone-parent my-cpp-func-sig-arg-indent)
;;       ((match ")" "parameter_list" nil nil nil) standalone-parent 0)
;;       ((match nil "parameter_list" nil 2 nil) (nth-sibling 1) 0)
;;       ((and no-node (parent-is "parameter_list")) (nth-sibling 1) 0)

;;       ;; fix indent for multi-line function invocation
;;       ((match nil "argument_list" nil 1 1) standalone-parent my-cpp-func-sig-arg-indent)
;;       ((match ")" "parameter_list" nil nil nil) standalone-parent 0)
;;       ((match nil "parameter_list" nil 2 nil) (nth-sibling 1) 0)
;;       ((and no-node (parent-is "parameter_list")) (nth-sibling 1) 0)

;;       ;; append custom rules to indent style serving as base
;;       ,@(alist-get 'linux (c-ts-mode--indent-styles 'cpp))))

;;   :config
;;   (setq-default c-ts-mode-indent-offset 2)
;;   (setq-default c-ts-mode-indent-style #'my-cpp-indent-style)
;;   (setq-default indent-tabs-mode nil))

;; (defun my-cpp-config-hook ()
;;   "fallback c++-mode basic configs (sans treesitter)"
;;   (setq-default c-basic-offset 2)
;;   ;; customized indentation
;;   (c-set-offset 'substatement-open 0)
;;   (c-set-offset 'arglist-intro 4))
;; (add-hook 'c++-mode-hook 'my-cpp-config-hook)

;; ------------------------------------------------------------------------
;; golang configs
;; ensure PATH / GOPATH properly set externally!
;; LSP: https://github.com/golang/tools/blob/master/gopls/doc/emacs.md
;; ------------------------------------------------------------------------
;; (use-package go-mode
;;   :ensure t
;;   :defer t
;;   :hook (go-ts-mode . my-go-cfg-hook)
;;   :config
;;   ;; goimports acts as superior replacement for gofmt
;;   ;; https://pkg.go.dev/golang.org/x/tools/cmd/goimports
;;   (setq-default gofmt-command "goimports")
;;   ;; fix dumb default indent settings
;;   (setq-default go-ts-mode-indent-offset 4))

;; (defun my-go-cfg-hook ()
;;   ;; hack: ensure go-mode load when opening in go-ts-mode
;;   (gofmt-before-save)
;;   ;; hack: gofmt-before-save hard-coded to only work on go-mode
;;   (add-hook 'before-save-hook 'gofmt nil t))

;; ;; org babel support for golang
;; ;; (use-package ob-go :ensure t)

;; ;; invoke gofmt after editing org src block via (C-c ')
;; (define-advice org-edit-src-exit (:before (&rest _args))
;;   (when (memq major-mode '(go-mode go-ts-mode)) (gofmt)))

;; ------------------------------------------------------------------------
;; javascript configs
;; LSP: https://github.com/typescript-language-server/typescript-language-server
;; ------------------------------------------------------------------------
;; auto format buffer with prettier
;; https://github.com/radian-software/apheleia
;; saving on remote files kinda sucks, so invoke manually with keybinding
(use-package apheleia
  :ensure t
  :config
  (setf (alist-get 'prettier-json apheleia-formatters)
        '("prettier" "--stdin-filepath" filepath)))

;; additional configs for JS editing
(defun my-js/ts-cfg-hook ()
  (local-set-key (kbd "C-c C-a") #'apheleia-format-buffer)
  (setq-default js-indent-level 2))
(add-hook 'typescript-ts-mode-hook #'my-js/ts-cfg-hook)

;; (use-package js2-mode :ensure t) ; for linting
;; (use-package flow-js2-mode :ensure t) ; supports js2 (needed for flow)

;; convenience mode for flow server interaction
;; C-c C-c [(s)tatus, (c)overage, (t)ype-at-point]
;; (use-package flow-minor-mode
;;   :ensure t
;;   :hook (flow-minor-mode . my-flow-save-check-hook)
;;   :bind (:map flow-minor-mode-map ("M-," . nil) ("M-." . nil)))

;; main configs for js editing (js-ts-mode as base)
;; (defun my-js-cfg-hook ()
;;   (local-unset-key (kbd "M-."))
;;   (flow-minor-mode)
;;   (flow-js2-mode)
;;   (js2-minor-mode))
;; (add-hook 'js-ts-mode-hook #'my-js-cfg-hook)

;; ;; look for locally installed eslint for use in flycheck
;; (defun my/use-eslint-from-node-modules ()
;;   (let* ((root (locate-dominating-file
;;                 (or (buffer-file-name) default-directory) "node_modules"))
;;          (eslint (and root (expand-file-name "node_modules/.bin/eslint" root))))
;;     (when (and eslint (file-executable-p eslint))
;;       (setq-local flycheck-javascript-eslint-executable eslint))))
;; (add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)

;; ;; automated flow checks (if present) during save
;; (defun my-flow-save-check-hook ()
;;   (add-hook 'after-save-hook 'flow-status nil t))

;; ;; configure flow output popup behavior
;; (add-to-list 'display-buffer-alist
;;              '("\\(\\*compilation\\*\\|\\*Flow Output\\*\\)"
;;                (display-buffer-reuse-window display-buffer-below-selected)
;;                (window-height . 0.15)))

;; ------------------------------------------------------------------------
;; hack configs
;; LSP: https://docs.hhvm.com/hhvm/installation/introduction ...
;; ------------------------------------------------------------------------
(use-package hack-mode
  :ensure t
  :hook  (hack-mode . my-hack-cfg-hook))

;; main configs for hack editing
(defun my-hack-cfg-hook ()
  (add-hook 'hack-mode-hook #'hack-enable-format-on-save nil t))

;; ------------------------------------------------------------------------
;; protobuf configs
;; ------------------------------------------------------------------------
;; ; 4-space indent
;; (defconst my-protobuf-style
;;   '((c-basic-offset . 4)
;;     (indent-tabs-mode . nil)))

;; (add-hook 'protobuf-mode-hook
;;           (lambda () (c-add-style "my-style" my-protobuf-style t)))

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
