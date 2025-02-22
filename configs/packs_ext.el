;; ------------------------------------------------------------------------
;; version control front-ends
;; ------------------------------------------------------------------------
;; git
(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status)
  :config
  (setq-default magit-diff-refine-hunk nil)
  (setq-default magit-log-margin '(t "%Y-%m-%d %H:%M " magit-log-margin-width t 12))
  (setq-default magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1))

;; mercurial
;; currently doesn't support staging individual hunks (bla)
;; https://github.com/ananthakumaran/monky/issues/104
;; instead resort to 'hg commit -i' and manual selection (ouch)
(use-package monky
  :ensure t
  :bind ("C-x m" . monky-status)
  :config
  (setq monky-process-type 'cmdserver))

;; configure monky popup behavior
(add-to-list 'display-buffer-alist
             '("\\*monky"
               (display-buffer-same-window display-buffer-reuse-window)))

;; ------------------------------------------------------------------------
;; vertico / marginalia / orderless / consult - completion framework
;; ------------------------------------------------------------------------
(use-package vertico ; completion UI based on default completion system
  :ensure t
  :config
  (vertico-mode)
  ;; add parens around number counts
  (setq-default vertico-count-format '("%-6s " . "(%s/%s)"))
  ;; situational display style: new buffer for grep related
  (vertico-multiform-mode)
  (setq-default vertico-multiform-commands '((consult-ripgrep buffer))))

(use-package savehist ; (native) persist minibuffer history
  :config
  (savehist-mode)
  (setq-default history-length 100))

(use-package marginalia ; annotations in minibuffer
  :ensure t
  ;; :custom (marginalia-align 'right)
  :config (marginalia-mode))

(use-package orderless ; fuzzy, unordered completion style
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package consult ; handy search / nav commands built on vertico
  :ensure t
  :bind
  (("C-x b" . consult-buffer) ; better switch-to-buffer (with preview)
   ;; M-g bindings in `goto-map'
   ("M-g o" . consult-outline) ; jump to heading (with preview)
   ("M-g g" . consult-goto-line) ; replacement of goto-line (with preview)
   ("M-g M-g" . consult-goto-line)
   ;; M-s bindings in `search-map'
   ("M-s g" . consult-ripgrep) ; super efficient regexp across files (with preview)
   ("M-s m" . consult-line-multi))) ; line search across open buffers (with preview)

;; ------------------------------------------------------------------------
;; corfu - in-buffer completion (not the minibuffer)
;; ------------------------------------------------------------------------
;; (use-package corfu
;;   :ensure t
;;   :bind (:map corfu-map ("RET" . nil)) ; tab only completion
;;   :custom
;;   ;; automatic completion
;;   (corfu-auto t) ; automatic completion popup
;;   (corfu-auto-prefix 3) ; min prefix length for popup to show
;;   (corfu-auto-delay 0.1) ; show completion popup faster when able
;;   (corfu-preselect 'first) ; first candidate selected; tab then inserts
;;   (corfu-count 8) ; max candidates to show in popup

;;   :config
;;   ;; enables secondary popup to completion popup for candidate docs
;;   ;; manually invoke via 'M-t' defined in corfu-popupinfo-map
;;   ;; similarly, navigate with 'M-<next>' (pageup) and 'M-<prior>' (pagedown)
;;   (setq-default corfu-popupinfo-delay nil) ; never auto popup candidate docs
;;   (corfu-popupinfo-mode 1)
;;   (if (eql dcw-dark-theme-flag t)
;;       (set-face-attribute 'corfu-current nil :background "#4d4d4d")
;;     (set-face-attribute 'corfu-current nil :background "gray80"))
;;   (set-face-attribute 'completions-common-part nil :foreground "#00EEEE")
;;   :init (global-corfu-mode))

;; ------------------------------------------------------------------------
;; swiper - search improvement
;; retaining since consult-line ordering is disagreeable
;; ------------------------------------------------------------------------
(use-package swiper
  :ensure t
  :bind
  (("C-s" . swiper-isearch-thing-at-point))
  :config
  (setq-default swiper-goto-start-of-match nil)
  (setq-default swiper-verbose nil)

  ;; don't quit search when backspace on empty query
  (setq-default ivy-on-del-error-function 'ignore)
  ;; default ivy frontend height
  (setq-default ivy-height 1)
  ;; remove all the swiper color loudness
  (if (eql dcw-dark-theme-flag t)
      (set-face-attribute 'swiper-background-match-face-2 nil :inherit 'default :background "#0e5e95")
    (set-face-attribute 'swiper-background-match-face-2 nil :inherit 'default :background "light blue"))

  (set-face-attribute 'ivy-cursor nil :foreground "black" :background "#ff8c00")
  (set-face-attribute 'swiper-background-match-face-1 nil :foreground 'unspecified :background 'unspecified)
  (set-face-attribute 'swiper-background-match-face-3 nil :background 'unspecified :inherit 'swiper-background-match-face-2)
  (set-face-attribute 'swiper-background-match-face-4 nil :background 'unspecified :inherit 'swiper-background-match-face-2)

  ;; remove inheritance to avoid theme line highlight color confusion
  (set-face-attribute 'swiper-line-face nil :inherit nil :underline nil :foreground 'unspecified :background 'unspecified))

;; ------------------------------------------------------------------------
;; lsp-mode - adds options for IDE-like features
;; ------------------------------------------------------------------------
(use-package lsp-ui
  :ensure t)

(use-package lsp-mode
  :ensure t
  :init
  ;; allow for substring + subsequence matching
  ;; https://github.com/minad/corfu/wiki
  (defun my-lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(flex))) ;; Configure flex
  :bind
  ;; default keymap prefix: 'super-l'
  ;; use ("," and ".") for navigation to immediately preview xref options
  ;; unfortunately opens buffers while doing so
  (("M-." . lsp-find-definition) ; M-, to return to previous point
   ("M-?" . lsp-find-references))

  :hook
  ((prog-mode . lsp)
   (lsp-completion-mode . my-lsp-mode-setup-completion)
   ;; auto-save buffers when post lsp-rename
   (lsp-after-apply-edits . (lambda (op) (when (eq op 'rename) (save-buffer))))
   ;; ensure doc popup has line numbers disabled
   (lsp-ui-doc-frame-mode . (lambda () (display-line-numbers-mode -1))))

  :config
  ;; don't log all message from language server (impacts performance)
  (setq-default lsp-log-io nil)
  ;; disable code lens overlay
  (setq-default lsp-lens-enable nil)
  ;; show doc overlay with mouse hover
  (setq-default lsp-ui-doc-show-with-mouse nil)
  ;; remove entire sideline
  (setq-default lsp-ui-sideline-enable nil)
  ;; remove modeline related diagnostics
  (setq-default lsp-modeline-code-actions-enable nil)
  (setq-default lsp-modeline-diagnostics-enable nil)
  (setq-default lsp-modeline-workspace-status-enable nil)
  ;; configure completion-at-point integration (using corfu)
  (setq-default lsp-completion-enable t)
  (setq-default lsp-completion-provider :none)
  ;; hide breadcrumb (top line) error diagnostic indication
  (setq-default lsp-headerline-breadcrumb-enable-diagnostics nil)
  ;; syntax checking: use neither flymake nor lsp
  (setq-default lsp-diagnostics-provider :none)
  ;; suppress warnings when no language server present
  (setq-default lsp-warn-no-matched-clients nil)
  ;; disable right click menu
  (define-key lsp-mode-map (kbd "<mouse-3>") nil)
  ;; disable function signature minibuffer popup (distracting)
  (setq-default lsp-signature-auto-activate nil)
  (setq-default lsp-signature-render-documentation nil)
  (setq-default lsp-eldoc-enable-hover nil)

  ;; python - constant linting causes visual flickering (so disable)
  (setq-default lsp-pylsp-plugins-flake8-enabled nil)
  (setq-default lsp-pyls-plugins-flake8-enabled nil)
  (setq-default lsp-pylsp-plugins-mccabe-enabled nil)
  (setq-default lsp-pyls-plugins-mccabe-enabled nil)

  ;; change some font faces
  (if (eql dcw-dark-theme-flag t)
      (set-face-attribute
       'lsp-face-highlight-textual nil
       :background "SlateBlue3" :foreground "gray90" :underline nil :bold nil)
    (set-face-attribute
     'lsp-face-highlight-textual nil
     :background "cyan2" :foreground "black" :underline nil :bold nil))
  (set-face-attribute
   'lsp-face-highlight-read nil
   :inherit 'lsp-face-highlight-textual :underline nil :bold nil)
  (set-face-attribute
   'lsp-face-highlight-write nil
   :inherit 'lsp-face-highlight-textual :underline nil :bold nil))

;; ------------------------------------------------------------------------
;; vterm - better terminal in emacs
;; https://github.com/akermu/emacs-libvterm
;; ------------------------------------------------------------------------
(use-package vterm
  :ensure t
  :bind (("C-x C-v" . vterm)) ; override find-alternate-file
  :hook (vterm-mode . my-darkmode-vterm-faces)
  :config
  (setq-default vterm-min-window-width 100) ; avoid line wrapping
  (setq-default vterm-copy-exclude-prompt t)
  (setq-default vterm-max-scrollback 10000)
  (setq-default vterm-buffer-name-string "vterm %s"))

(defun my-darkmode-vterm-faces()
  ;; make dark colored faces legible when using dark mode themes"
  (if (eql dcw-dark-theme-flag t)
      (progn
        (set-face-attribute 'ansi-color-bright-black nil :foreground "#eee8cd")
        (set-face-attribute 'vterm-color-black nil :foreground "#7f7f7f"))))

;; ------------------------------------------------------------------------
;; avy - jump to char!
;; ------------------------------------------------------------------------
(use-package avy
  :ensure t
  :bind ("C-f" . 'avy-goto-char-timer)
  :custom-face
  (avy-lead-face ((t (:foreground "#00ffff" :background "#000000"))))
  (avy-lead-face-0 ((t (:foreground "#1297b9" :background "#000000"))))
  (avy-lead-face-1 ((t (:foreground "#1297b9" :background "#000000"))))
  (avy-lead-face-2 ((t (:foreground "#1297b9" :background "#000000"))))
  :config
  (setq-default avy-timeout-seconds 0.30)
  (setq-default avy-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (setq-default avy-style 'de-bruijn)
  (setq-default avy-background nil))

;; ------------------------------------------------------------------------
;; flycheck - code syntax checking
;; requires local installation of checking program (flake8 / mypy etc.)
;; https://www.flycheck.org/en/latest/languages.html#python
;; ------------------------------------------------------------------------
(use-package flycheck
  :ensure t
  :bind ("C-c ! !" . flycheck-mode)
  ;; :hook (prog-mode . flycheck-mode)
  :config
  (setq-default flycheck-relevant-error-other-file-show nil)
  (setq-default flycheck-check-syntax-automatically '(mode-enabled save))

  ;; configure checker for javascript in web-mode
  (flycheck-add-mode 'javascript-eslint 'web-mode)

  ;; configure checker order for python
  (setq-default flycheck-python-mypy-executable "mypy")
  (setq-default flycheck-python-flake8-executable "python3")
  (flycheck-add-next-checker 'python-flake8 'python-mypy)

  ;; remove visual clutter - indicate errors only
  (set-face-attribute 'flycheck-info nil :underline nil)
  (set-face-attribute 'flycheck-warning nil :underline nil)
  (set-face-attribute 'flycheck-fringe-info nil :foreground "gray20")

  ;; configure error list popup behavior
  (add-to-list 'display-buffer-alist
               '("*Flycheck errors*"
                 (display-buffer-reuse-window display-buffer-below-selected)
                 (window-height . 0.3))))

;; handy shortcuts:
;; C-c ! v = verify setup
;; C-c ! l = list of errors in buffer

;; ------------------------------------------------------------------------
;; ace-window - better window management
;; ------------------------------------------------------------------------
(use-package ace-window
  :ensure t
  :bind
  ("M-o" . ace-window)
  :config
  (setq-default aw-background nil)
  (setq-default aw-scope 'frame)
  (setq-default aw-dispatch-always t))

;; ------------------------------------------------------------------------
;; expand region - magic selection
;; ------------------------------------------------------------------------
(use-package expand-region
  :ensure t
  :bind ("C-=" . er/mark-symbol-with-prefix)) ;; er/expand-region can be slow

;; ------------------------------------------------------------------------
;; multiple cursors
;; active region == select stuff
;; exit with 'C-g' to trigger mode disabled hook (return inconsistent)
;; ------------------------------------------------------------------------
(use-package multiple-cursors
  :ensure t
  :hook
  ((multiple-cursors-mode . my-mc-config-start-hook)
   (multiple-cursors-mode-disabled . my-mc-config-end-hook))
  :custom
  (mc/match-cursor-style nil) ; disable fake cursor matching, shifts text
  :bind
  (("C->" . mc/edit-beginnings-of-lines) ; drops cursor on all region lines
   ("C-c C->" . mc/mark-all-like-this) ; all matches of active region
   ;; ensure same behavior when exiting with 'return' / 'C-g'
   :map mc/keymap ("<return>" . 'mc/keyboard-quit)))

;; superword treats "some_words" as single logical symbol
(defun my-mc-config-start-hook ()
  (superword-mode 1))
(defun my-mc-config-end-hook ()
  (superword-mode -1))

;; ------------------------------------------------------------------------
;; crux - https://github.com/bbatsov/crux
;; ------------------------------------------------------------------------
(use-package crux
  :ensure t
  :bind
  ("C-a" . crux-move-beginning-of-line) ; move to first char, repeat to gutter
  ("M-k" . crux-kill-line-backwards)) ; kill from point towards gutter

;; ------------------------------------------------------------------------
;; yasnippet - template system (automate boilerplate stuff)
;; ------------------------------------------------------------------------
;; (use-package yasnippet
;;   :ensure t
;;   :config
;;   (setq-default yas-verbosity 1) ;; step above nothing

;;   ;; use yasnippet on per-buffer basis (based on major mode)
;;   (yas-reload-all)
;;   :hook (prog-mode . yas-minor-mode))

;; ------------------------------------------------------------------------
;; helpful - better help command buffers
;; ------------------------------------------------------------------------
(use-package helpful
  :ensure t
  :bind
  (("C-h f" . helpful-callable)
   ("C-h v" . helpful-variable)
   ("C-h k" . helpful-key)
   ("C-h x" . helpful-command)
   ("C-h h" . helpful-at-point)))

;; ------------------------------------------------------------------------
;; deadgrep - better text search through files
;; needs ripgrep: https://github.com/BurntSushi/ripgrep#installation
;; keybindings: https://github.com/Wilfred/deadgrep#keybindings
;; ------------------------------------------------------------------------
(use-package deadgrep
  :ensure t
  :bind
  ("<f3>" . deadgrep))

;; ------------------------------------------------------------------------
;; rainbow-mode - colorize text codes; handy when changing colors
;; ------------------------------------------------------------------------
(use-package rainbow-mode
  :ensure t)

;; ------------------------------------------------------------------------
;; diredfl - add font lock color rules for dired
;; ------------------------------------------------------------------------
(use-package diredfl
  :ensure t
  :hook (dired-mode . diredfl-mode))

;; ------------------------------------------------------------------------
;; unfill
;; ------------------------------------------------------------------------
(use-package unfill
  :ensure t
  :bind
  (("C-c e" . my-copy-unfilled-for-export)))

(defun my-copy-unfilled-for-export ()
  "Copy unfilled region to clipboard and leave buffer in initial state"
  (interactive)
  (if (use-region-p)
      (let ((region-start (region-beginning)) (region-end (region-end)))
        (let ((original-text (buffer-substring-no-properties region-start region-end)))
          (with-temp-buffer
            (org-mode) ; set temp buffer major mode
            (insert original-text)
            ;; unfill-region / copy to clipboard
            (let ((start (point-min)) (end (point-max))) (unfill-region start end))
            ;; Copy the processed text to the clipboard
            (kill-new (buffer-string))))
        (message "Unfilled region copied to clipboard.")
        (deactivate-mark))
    (message "No region selected")))
