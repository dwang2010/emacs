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
  :hook (dired-mode . dired-hide-details-mode) ; collapsed extra info, show via "("
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
;; project based file management (native)
;; ------------------------------------------------------------------------
(use-package project ; test native
  ;; native binds
  ;; C-x p d == (dired to target dir)
  ;; C-x p D == (dired to root dir)
  :config
  (setq-default project-vc-ignores '()) ; list of stuff to ignore
  (setq-default project-switch-commands
                '((project-find-file "Find file" nil)
                  (project-find-dir "Find directory" nil))))

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
  (vertico-multiform-mode))
  (setq-default vertico-multiform-commands '((consult-grep buffer)))

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
   ("M-s g" . consult-grep) ; super efficient regexp across files (with preview)
   ("M-s m" . consult-line-multi))) ; line search across open buffers (with preview)

;; ------------------------------------------------------------------------
;; corfu - in-buffer completion (not the minibuffer)
;; ------------------------------------------------------------------------
(use-package corfu
  :ensure t
  :bind
  ;; tab only completion (remove enter key)
  (:map corfu-map ("RET" . nil))
  :custom
  ;; disable automatic completion popup
  (corfu-auto nil)
  ;; max candidates to show in popup
  (corfu-count 8)
  ;; tabs both indent and complete
  (tab-always-indent 'complete)
  :init
  (global-corfu-mode))

;; ------------------------------------------------------------------------
;; swiper - search improvement
;; retaining since consult-line ordering is disagreeable
;; ------------------------------------------------------------------------
(use-package swiper
  :ensure t
  :bind
  (("C-s" . swiper-isearch)
   ("C-r" . swiper-isearch-thing-at-point))
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
  :init
  (setq-default lsp-keymap-prefix "C-c l")
  ;; allow for substring + subsequence matching
  ;; https://github.com/minad/corfu/wiki
  (defun my-lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(flex))) ;; Configure flex
  :bind (("M-." . lsp-find-definition)
         ("M-?" . lsp-find-references))
  :config
  ;; don't log all message from language server (impacts performance)
  (setq-default lsp-log-io nil)
  ;; disable code lens overlay
  (setq-default lsp-lens-enable nil)
  ;; remove doc dialogue on all hover (keyboard / mouse)
  (setq-default lsp-ui-doc-enable nil)
  ;; remove entire sideline
  (setq-default lsp-ui-sideline-enable nil)
  ;; remove modeline related diagnostics
  (setq-default lsp-modeline-code-actions-enable nil)
  (setq-default lsp-modeline-diagnostics-enable nil)
  (setq-default lsp-modeline-workspace-status-enable nil)
  ;; remove function signature help docs
  (setq-default lsp-signature-render-documentation nil)
  ;; configure completion-at-point integration (using corfu)
  (setq-default lsp-completion-enable t)
  (setq-default lsp-completion-provider :none)
  ;; hide breadcrumb (top line) error diagnostic indication
  (setq-default lsp-headerline-breadcrumb-enable-diagnostics nil)
  ;; disable flake8 plugin - constant linting causes visual flickering
  (setq-default lsp-pylsp-plugins-flake8-enabled nil)
  (setq-default lsp-pyls-plugins-flake8-enabled nil)
  ;; syntax checking: use neither flymake nor lsp
  (setq-default lsp-diagnostics-provider :none)
  ;; suppress warnings when no language server present
  (setq-default lsp-warn-no-matched-clients nil)
  ;; disable right click menu
  (define-key lsp-mode-map (kbd "<mouse-3>") nil)

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

  :hook ((prog-mode . lsp)
         (lsp-completion-mode . my-lsp-mode-setup-completion)))

;; ------------------------------------------------------------------------
;; vterm - better terminal in emacs
;; https://github.com/akermu/emacs-libvterm
;; ------------------------------------------------------------------------
(use-package vterm
  :ensure t
  :bind (("C-x C-v" . vterm)) ; override find-alternate-file
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
  (setq-default org-pretty-entities nil) ;; don't show special utf-8 symbols

  ;; customize heading sizes
  (set-face-attribute 'org-level-1 nil :height 1.15)
  (set-face-attribute 'org-level-2 nil :height 1.10)
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
   '((python . t)
     (lisp . t)))
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
          (wl . wl-other-frame)))

  ;; tags related
  (setq-default org-tags-column -80)
  (setq-default org-auto-align-tags t)

  ;; automatically add completion time when closing TODO
  (setq-default org-log-done 'time)

  ;; org-agenda related
  (setq-default org-agenda-files '("~/notes/capture.org"))
  (setq-default org-agenda-window-setup 'current-window)

  ;; org-capture - allows "capture" of notes at any time, quickly!
  ;; use tags to add additional context for filtering
  ;; e.g. "project" or "attendees" or "meeting_type" etc.
  (global-set-key (kbd "<f2>") 'org-capture)
  (setq-default org-default-notes-file "~/notes/capture.org")
  (setq org-capture-templates
        '(;; task, no reference
          ("t" "Task" entry
           (file+headline org-default-notes-file "Tasks")
           "* TODO [#2] %?\nDEADLINE: " :empty-lines-after 1 :prepend t)

          ;; task with code reference
          ("c" "Task w/ Code Ref" entry
           (file+headline org-default-notes-file "Tasks w/ Ref")
           "* TODO [#2] %?\nDEADLINE: \n%a" :empty-lines-after 1 :prepend t)

          ;; meeting related notes
          ("m" "Meeting Notes" entry
           (file+headline org-default-notes-file "Meeting Notes")
           "* %t %^{Name}\n%?" :empty-lines-after 1 :prepend t)

          ;; backlog of random thoughts / ideas
          ("b" "Backlog" entry
           (file+headline org-default-notes-file "Backlog")
           "* TODO %?" :prepend t)))

  ;; org-refile - for moving stuff between org files
  (setq-default org-reverse-note-order t) ; prepend on refile
  (setq-default org-blank-before-new-entry nil) ; prepend doesn't need another blank
  (setq-default org-refile-use-outline-path 'file) ; allow refiling to other files
  (setq-default org-outline-path-complete-in-steps nil) ; show all the options

  ;; specific predefined targets for refiling (add as needed)
  (setq-default org-refile-targets '(("~/notes/capture.org" :maxlevel . 3))))

;; make org bullets fancy
(use-package org-superstar
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-superstar-mode 1))))

;; more evident task priority
(use-package org-fancy-priorities
  :ensure t
  :hook (org-mode . org-fancy-priorities-mode)
  :config
  (setq-default org-priority-highest 0
                org-priority-default 1
                org-priority-lowest 2)
  (setq-default org-fancy-priorities-list '((?0 . "P0")
                                            (?1 . "P1")
                                            (?2 . "P2")))
  (setq-default org-priority-faces '((?0 :foreground "#ff3030" :background nil)
                                     (?1 :foreground "#eeb422" :background nil)
                                     (?2 :foreground "#b3b3b3" :background nil))))

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
