;; ------------------------------------------------------------------------
;; paren - highlight matching braces (native)
;; ------------------------------------------------------------------------
(use-package paren
  :config (show-paren-mode 1)
  (setq-default show-paren-delay 0)
  (set-face-attribute 'show-paren-match nil :bold t))

;; ------------------------------------------------------------------------
;; dired - file / directory explorer (native)
;; ------------------------------------------------------------------------
(use-package dired
  :bind ("C-x C-d" . 'dired-jump) ; jump straight to dir of current buffer
  :config
  (setq-default dired-dwim-target t) ; target operation to other dired window
  (setq-default dired-listing-switches "-laGh --group-directories-first -v")
  (setq-default dired-kill-when-opening-new-dired-buffer t))

(require 'ls-lisp)
(setq-default ls-lisp-use-insert-directory-program nil)
(setq-default ls-lisp-dirs-first t)

;; ------------------------------------------------------------------------
;; highlight current line only in specific modes (native)
;; ------------------------------------------------------------------------
(use-package hl-line
  :hook ((prog-mode . hl-line-mode)
         (text-mode . hl-line-mode)))

;; ------------------------------------------------------------------------
;; hideshow - code folding (native)
;; ------------------------------------------------------------------------
(use-package hideshow
  :bind (("<C-tab>" . hs-toggle-hiding)
         ("<C-M-tab>" . 'hs-hide-all))
  :hook ((prog-mode . hs-minor-mode)
         (c-mode-common . hs-minor-mode)))

;; ------------------------------------------------------------------------
;; flyspell - spell checking (native)
;; ispell needs to be locally installed (not a part of emacs)
;; ------------------------------------------------------------------------
(use-package flyspell
  :bind ("C-c C-4" . 'flyspell-mode)
  :config (setq-default flyspell-issue-message-flag nil))

;; M-$ = Check and correct spelling of the word at point
;; flyspell-buffer to check spelling of entire buffer

;; ------------------------------------------------------------------------
;; hippie expand - text completion (native)
;; ------------------------------------------------------------------------
;; use hippie-expand instead of dabbrev (built-in expansion / completion)
;; https://www.masteringemacs.org/article/text-expansion-hippie-expand
(use-package hippie-expand
  :bind ("M-/" . hippie-expand))

;; ------------------------------------------------------------------------
;; tree sitter - syntax highlighting (native in emacs >= 29)
;; ------------------------------------------------------------------------
(use-package treesit
  :config
  (setq-default treesit-font-lock-level 4))

;; deal with the new major modes
;; https://github.com/renzmann/treesit-auto
(use-package treesit-auto
  :ensure t
  :config
  (setq-default treesit-auto-install 'prompt)
  (global-treesit-auto-mode))

;; ------------------------------------------------------------------------
;; rainbow-mode - colorize text codes; handy when changing colors
;; ------------------------------------------------------------------------
(use-package rainbow-mode
  :ensure t)

;; ------------------------------------------------------------------------
;; expand region - magic selection
;; ------------------------------------------------------------------------
(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

;; ------------------------------------------------------------------------
;; avy - jump to char!
;; ------------------------------------------------------------------------
(use-package avy
  :ensure t
  :bind ("C-f" . 'avy-goto-char-timer)
  :config (setq-default avy-timeout-seconds 0.20)
  (setq-default avy-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (setq-default avy-style 'de-bruijn)
  (setq-default avy-background nil))

;; ------------------------------------------------------------------------
;; projectile - project based file management, very handy
;; ------------------------------------------------------------------------
(use-package projectile
  :ensure t
  :init (projectile-mode +1)
  :bind ("C-x p" . projectile-command-map))

;; ------------------------------------------------------------------------
;; ivy / counsel / swiper - minibuffer / completion / search improvement
;; https://github.com/abo-abo/swiper
;; ------------------------------------------------------------------------
(use-package ivy
  :ensure t
  :custom
  (ivy-count-format "(%d/%d) ")
  :config (ivy-mode)
  (setq-default ivy-on-del-error-function #'ignore)
  ;; swiper style color matching
  (setq-default ivy-display-style 'fancy)
  (setq-default ivy-height 8))

(use-package ivy-rich ; UI enhancements
  :ensure t
  :after ivy
  :config
  (ivy-rich-mode 1)
  ;; fix to ensure line highlight completely extended
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line))

(use-package counsel
  :ensure t
  :after ivy
  :config (counsel-mode))

(use-package swiper
  :ensure t
  :after ivy
  :bind (("C-s" . swiper-isearch)
         ("C-r" . swiper-backward)
         ("C-c C-w" . swiper-isearch-thing-at-point))
  :config
  ;; color contrast between selected / other options (similar to vanilla)
  (set-face-attribute 'swiper-background-match-face-2 nil :inherit 'swiper-match-face-1)
  ;; remove inheritance to avoid theme line highlight color confusion
  (set-face-attribute 'swiper-line-face nil :inherit nil :underline nil))

;; ------------------------------------------------------------------------
;; magit - better git
;; ------------------------------------------------------------------------
(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status)
  :config
  (setq-default magit-log-margin '(t "%Y-%m-%d %H:%M " magit-log-margin-width t 12))
  (setq-default magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1))

;; ------------------------------------------------------------------------
;; lsp-mode - adds options for IDE-like features
;; ------------------------------------------------------------------------
(use-package lsp-mode
  :ensure t
  :init (setq-default lsp-keymap-prefix "C-c l")
  :bind (("M-." . lsp-find-definition)
         ("M-?" . lsp-find-references))
  :config
  ;; disable a bunch of unneeded stuff
  (setq-default lsp-log-io nil)
  (setq-default lsp-lens-enable nil)
  (setq-default lsp-ui-doc-enable nil)
  (setq-default lsp-ui-sideline-enable nil)
  (setq-default lsp-ui-sideline-show-code-actions nil)
  (setq-default lsp-ui-sideline-show-hover nil)
  (setq-default lsp-ui-sideline-show-diagnostics nil)
  (setq-default lsp-modeline-code-actions-enable nil)
  (setq-default lsp-modeline-diagnostics-enable nil)
  (setq-default lsp-modeline-workspace-status-enable nil)
  (setq-default lsp-signature-render-documentation nil)
  (setq-default lsp-diagnostics-package :none)
  (setq-default lsp-diagnostics-provider :none)
  (setq-default lsp-completion-provider :none)
  (setq-default lsp-completion-show-detail nil)
  (setq-default lsp-completion-show-kind nil)
  (setq-default lsp-completion-enable nil)
  (setq-default lsp-headerline-breadcrumb-enable-diagnostics nil)
  (setq-default lsp-enable-snippet nil)
  (define-key lsp-mode-map (kbd "<mouse-3>") nil) ;; disables right click menu

  ;; constantly linting despite being off, very annoying
  (setq-default lsp-pylsp-plugins-flake8-enabled nil)
  ;; (setq-default lsp-pyls-plugins-flake8-enabled nil)

  ;; change some font faces
  (set-face-attribute
   'lsp-face-highlight-textual nil
   :background "SlateBlue3" :foreground "gray90" :underline nil :bold nil)
  (set-face-attribute
   'lsp-face-highlight-read nil
   :inherit 'lsp-face-highlight-textual :underline nil :bold nil)
  (set-face-attribute
   'lsp-face-highlight-write nil
   :inherit 'lsp-face-highlight-textual :underline nil :bold nil)

  ;; suppress warnings when no language server present
  (setq-default lsp-warn-no-matched-clients nil)

  :hook ((prog-mode . lsp)))

;; ------------------------------------------------------------------------
;; vterm - better terminal in emacs
;; https://github.com/akermu/emacs-libvterm
;; ------------------------------------------------------------------------
(use-package vterm
  :ensure t
  :config
  (setq-default vterm-copy-exclude-prompt t))

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
;; org-mode - note taking on steroids
;; ------------------------------------------------------------------------
(use-package org
  :ensure t
  :config
  ;; startup behavior
  (setq-default org-startup-indented t)
  (setq-default org-indent-indentation-per-level 1)

  ;; customize heading sizes
  (set-face-attribute 'org-level-1 nil :height 1.25)
  (set-face-attribute 'org-level-2 nil :height 1.15)
  (set-face-attribute 'org-level-3 nil :height 1.05)
  (set-face-attribute 'org-level-4 nil :height 1.0)

  ;; customize emphasis markers
  (setq-default org-hide-emphasis-markers t)
  (setq-default org-emphasis-alist
        '(("*" (bold :foreground "orange"))
          ("/" italic)
          ("_" underline)
          ("=" (:background "maroon" :foreground "white"))
          ("~" (:background "deep sky blue" :foreground "MidnightBlue"))
          ("+" (:strike-through t))))

  ;; babel configuration to load languages
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)))

  ;; set org-babel for desired python executable
  (setq-default org-babel-python-command "python3")

  ;; potentially risky: remove confirmation when executing code blocks
  (setq-default org-confirm-babel-evaluate nil)

  ;; C-c C-o opens link in same buffer rather than new buffer
  (setq-default org-link-frame-setup
        '((vm . vm-visit-folder-other-frame)
          (vm-imap . vm-visit-imap-folder-other-frame)
          (gnus . org-gnus-no-new-news)
          (file . find-file)
          (wl . wl-other-frame))))

;; make org bullets fancy
(use-package org-superstar
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-superstar-mode 1))))

;; ------------------------------------------------------------------------
;; flycheck - code syntax checking
;; requires local installation of checking program (flake8 / mypy etc.)
;; https://www.flycheck.org/en/latest/languages.html#python
;; ------------------------------------------------------------------------
(use-package flycheck
  :ensure t
  :bind
  ("C-c ! !" . flycheck-mode)
  ;; :hook (prog-mode . flycheck-mode) ;; off until desired
  :config
  (setq-default flycheck-relevant-error-other-file-show nil)
  (setq-default flycheck-check-syntax-automatically '(mode-enabled save)))

;; handy shortcuts:
;; C-c ! v = verify setup
;; C-c ! l = list of errors in buffer

;; ------------------------------------------------------------------------
;; yasnippet - template system (automate boilerplate stuff)
;; ------------------------------------------------------------------------
(use-package yasnippet
  :ensure t
  :config
  (setq-default yas-verbosity 1) ;; step above nothing

  ;; use yasnippet on per-buffer basis (based on major mode)
  (yas-reload-all)
  :hook (prog-mode . yas-minor-mode))
