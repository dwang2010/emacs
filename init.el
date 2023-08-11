(require 'package)

;; add melpa repo for packages
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; reduce frequency of garbage collection; happen on 50MB of allocated data
(setq-default gc-cons-threshold 50000000) ;; 50MB

;; increase data which emacs reads (lsp server)
(setq-default read-process-output-max (* 1024 1024)) ;; 1mb

;; goodbye to custom-set-variable mutations (just don't load)
(setq-default custom-file "~/.emacs.d/custom.el")

;; ------------------------------------------------------------------------
;; use package configs
;; ------------------------------------------------------------------------
;; make sure use-package is installed
(unless (package-installed-p 'use-package) (package-install 'use-package))

(require 'use-package)
(setq-default use-package-verbose t)

;; ------------------------------------------------------------------------
;; base visual theme, choose one
;; ------------------------------------------------------------------------
;; ;; https://gitlab.com/aimebertrand/timu-macos-theme
;; (use-package timu-macos-theme
;;   :ensure t
;;   :config (load-theme 'timu-macos t))

;; https://github.com/purcell/color-theme-sanityinc-tomorrow
;; options: day, night, blue, bright, eighties
(use-package color-theme-sanityinc-tomorrow
  :ensure t)

;; https://protesilaos.com/emacs/ef-themes-pictures
(use-package ef-themes
  :ensure t
  :config
  (load-theme 'ef-maris-dark :no-confirm))

;; ------------------------------------------------------------------------
;; modify default behaviors
;; ------------------------------------------------------------------------
;; disable menu bar
(menu-bar-mode -1)

;; disable tool bar
(tool-bar-mode -1)

;; disable vertical scrollbars
(scroll-bar-mode -1)

;; remove trailing whitespace on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; ensure newline at end of file if not present
(setq-default require-final-newline t)

;; default coding system
(prefer-coding-system 'utf-8)

;; enable line numbers
(global-display-line-numbers-mode)

;; remove startup screen
(setq-default inhibit-startup-screen t)

;; enable column numbers
(setq-default column-number-mode t)

;; default fill mode width
(setq-default fill-column 100)

;; spaces instead of tabs when indenting
(setq-default indent-tabs-mode nil)

;; default number of spaces for tab
(setq-default tab-width 4)

;; show filename in title bar
(setq-default frame-title-format "%b")

;; startup scratch message
(setq-default initial-scratch-message "")

;; startup major mode
(setq-default initial-major-mode 'text-mode)

;; major mode for new buffers
(setq-default major-mode 'text-mode)

;; remove emacs bell noise
(setq-default visible-bell 1)

;; disable visible bell flashing
(setq-default ring-bell-function 'ignore)

;; allow region uppercase / lowercase
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; backups in emacs directory
(setq-default backup-directory-alist '(("." . "~/.emacs.d/backups")))

;; exit confirmation (since slippery fingers sometimes)
(setq-default confirm-kill-emacs #'y-or-n-p)

;; have completions minibuffer sort stuff vertically
(setq-default completions-format 'vertical)

;; ensure scrollbars don't appear when creating new frames
(defun rmv-scroll-bars (frame)
  (modify-frame-parameters frame
                           '((vertical-scroll-bars . nil)
                             (horizontal-scroll-bars . nil))))
(add-hook 'after-make-frame-functions 'rmv-scroll-bars)

;; turn on hl-line
(global-hl-line-mode 1)

;; remove window retiling gaps
(setq-default frame-resize-pixelwise t)

;; truncate lines (do not line wrap)
(setq-default truncate-lines t)

;; overwrite currently selected region - handy for expand region
(delete-selection-mode t)

;; only one space after sentence for fill paragraph
(setq-default sentence-end-double-space nil)

;; simple y or no
(defalias 'yes-or-no-p 'y-or-n-p)

;; ------------------------------------------------------------------------
;; mouse behavior
;; ------------------------------------------------------------------------
(setq-default mouse-wheel-progressive-speed nil)
(if (not (eql system-type 'darwin)) (setq-default mouse-wheel-scroll-amount '(5)))

;; ------------------------------------------------------------------------
;; mode line customization
;; ------------------------------------------------------------------------
(setq-default display-time-format "(%a, %m/%d) | %I:%M %p") ; display time
(setq-default display-time-mail-string "")           ; remove mail notice
(setq-default display-time-default-load-average nil) ; remove load average
(display-time-mode nil)

(let ((standard-mode-line-format
       (list "%e"
             'mode-line-front-space
             'mode-line-mule-info
             'mode-line-client
             'mode-line-modified
             'mode-line-remote
             'mode-line-frame-identification
             ;'mode-line-buffer-identification
             ;"   "
             '(:eval (propertize "%b  " 'face 'mode-line-buffer-id))
             ;'mode-line-position
             "(%l,%c)  "
             ;'(:eval (propertize "(%l,%c)  " 'face 'mode-line))
             ;'(vc-mode vc-mode)
             ;"  "
             ;'mode-line-modes
             '(:eval (propertize "%m  " 'face 'shadow))
             'mode-line-misc-info
             ;'(:eval (propertize (format-time-string "(%a, %m/%d | %I:%M %p)") 'face 'shadow))
             'mode-line-end-spaces)))
  (setq-default mode-line-format standard-mode-line-format))

;; ------------------------------------------------------------------------
;; adjusted key bindings
;; ------------------------------------------------------------------------
;; bind align-regexp
(global-set-key (kbd "C-c a") 'align-regexp)

;; bind replace-string
(global-set-key (kbd "C-c s") 'replace-string)

;; bind string-rectangle
(global-set-key (kbd "C-c r") 'string-rectangle)

;; bind ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; unbind unused hotkeys
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))
(global-unset-key (kbd "C-x C-d"))
(global-unset-key (kbd "C-x C-c"))

;; also have C-x C-d bound to dired (for lazy hands)
(global-set-key (kbd "C-x C-d") 'dired)

;; change C-x s to same as C-x C-s
(global-set-key (kbd "C-x s") 'save-buffer)

;; logical switching between visible window panes
(global-set-key (kbd "<C-M-left>") 'windmove-left)
(global-set-key (kbd "<C-M-right>") 'windmove-right)
(global-set-key (kbd "<C-M-up>") 'windmove-up)
(global-set-key (kbd "<C-M-down>") 'windmove-down)

;; home / end keys to start / end of line instead of buffer
(global-set-key (kbd "<home>") 'move-beginning-of-line)
(global-set-key (kbd "<end>") 'move-end-of-line)

;; bind revert-buffer to F5 ("refresh")
(global-set-key (kbd "<f5>") 'revert-buffer)

;; bind eval-buffer to F12
(global-set-key (kbd "<f12>") 'eval-buffer)

;; mac specific keybinds
(if (eql system-type 'darwin)
    (progn
      (setq-default mac-option-modifier 'meta)
      (setq-default mac-command-modifier 'hyper)
      (global-set-key [(hyper f)] 'toggle-frame-fullscreen)
      (global-set-key [(hyper m)] 'toggle-frame-maximized)))

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
  :config (put 'dired-find-alternate-file 'disabled nil) ; "a" reuses current buffer
  (setq-default dired-dwim-target t) ; target operation to other dired window
  (setq-default dired-listing-switches "-laGh --group-directories-first -v"))

(require 'ls-lisp)
(setq-default ls-lisp-use-insert-directory-program nil)
(setq-default ls-lisp-dirs-first t)

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
  (setq-default avy-background nil)
  (set-face-attribute 'avy-lead-face nil :background "red2" :foreground "white")
  (set-face-attribute 'avy-lead-face-0 nil :background "saddle brown" :foreground "white")
  (set-face-attribute 'avy-lead-face-1 nil :background "#008b8b" :foreground "black")
  (set-face-attribute 'avy-lead-face-2 nil :background "saddle brown" :foreground "white"))

;; ------------------------------------------------------------------------
;; projectile - project based file management, very handy
;; ------------------------------------------------------------------------
(use-package projectile
  :ensure t
  :init (projectile-mode +1)
  :bind ("C-x p" . projectile-command-map))

;; ------------------------------------------------------------------------
;; ivy / swiper - minibuffer improvement & completion / better isearch
;; ------------------------------------------------------------------------
(use-package ivy
  :ensure t
  :config (ivy-mode)
  (setq-default ivy-on-del-error-function #'ignore)
  (setq-default ivy-display-style 'fancy)
  (setq-default ivy-height 4))

(use-package swiper
  :ensure t
  :after ivy
  :bind (("C-s" . swiper-isearch)
         ("C-r" . swiper-backward)
         ("C-c C-w" . swiper-isearch-thing-at-point))
  :config
  (set-face-attribute 'swiper-background-match-face-2 nil :inherit 'swiper-match-face-1)
  (set-face-attribute 'swiper-line-face nil :underline nil))

;; ------------------------------------------------------------------------
;; magit - better git
;; ------------------------------------------------------------------------
(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status)
  :config
  (setq-default magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1))

;; ------------------------------------------------------------------------
;; tree sitter - syntax highlighting (native in emacs >= 29)
;; ------------------------------------------------------------------------
;; first install language grammars (run script) and define location
;; https://github.com/casouri/tree-sitter-module
(use-package treesit
  :config
  (setq-default treesit-extra-load-path '("~/src/tree-sitter-module/dist"))
  (setq-default treesit-font-lock-level 4))

;; deal with the new major modes
;; https://github.com/renzmann/treesit-auto
(use-package treesit-auto
  :ensure t
  :config
  (global-treesit-auto-mode))

;; ------------------------------------------------------------------------
;; lsp-mode - adds IDE-like features
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
  (setq-default lsp-pyls-plugins-flake8-enabled nil)

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
  ;; customize emphasis markers
  (setq-default org-hide-emphasis-markers t)
  (setq-default org-emphasis-alist
        '(("*" (bold :foreground "orange"))
          ("/" italic)
          ("_" underline)
          ("=" (:background "maroon" :foreground "white"))
          ("~" (:background "deep sky blue" :foreground "MidnightBlue"))
          ("+" (:strike-through t))))

  ;; no indents on code block
  (setq-default org-src-preserve-indentation t)

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

;; ------------------------------------------------------------------------
;; python configs
;; ------------------------------------------------------------------------
;; configure location of python interpretter (system dependent)
(if (eql system-type 'darwin)
    (setq-default python-shell-interpreter "/opt/homebrew/bin/python3.10")
  (setq-default python-shell-interpreter "/usr/bin/python3"))

(setq-default python-shell-completion-native-enable nil)

;; ------------------------------------------------------------------------
;; golang configs
;; ------------------------------------------------------------------------
;(if (eql system-type 'darwin) (add-hook 'before-save-hook 'gofmt-before-save))

; env related workaround
(if (eql system-type 'darwin)
    (progn
      (setenv "PATH" (concat (getenv "PATH") ":/opt/homebrew/bin/"))
      (setq-default exec-path (append exec-path '("/opt/homebrew/bin/")))))

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
;; custom func: copy file name to clipboard
;; ------------------------------------------------------------------------
(defun copy-file-name-to-clipboard (do-not-strip-prefix)
  "Copy the current buffer file name to the clipboard. The path
   will be relative to the project's root directory, if
   set. Invoking with a prefix argument copies the full path."
  (interactive "P")
  (letrec
      ((fullname (if (equal major-mode 'dired-mode) default-directory (buffer-file-name)))
       (root (project-root (project-current)))
       (relname (file-relative-name fullname root))
       (should-strip (and root (not do-not-strip-prefix)))
       (filename (if should-strip relname fullname)))
    (kill-new filename)
    (message "Copied buffer file name '%s' to the clipboard." filename)))

(global-set-key (kbd "C-c C-y") #'copy-file-name-to-clipboard)

;; ------------------------------------------------------------------------
;; display customization
;; ------------------------------------------------------------------------
;; default font face, based on OS
(if (eql system-type 'darwin)
    (progn
      (set-face-attribute 'default nil :height 150)
      (setq-default line-spacing nil))
  (progn
    (set-face-attribute 'default nil :family "Consolas" :height 120)
    (setq-default line-spacing nil)))

;; default frame parameters
(setq-default default-frame-alist
              '((width . 100)              ; window width (chars, less line numbers)
                (height . 60)              ; window height (rows)
                (cursor-type . bar)        ; vertical bar cursor
                (cursor-color . "#ff7f00") ; cursor color
                ;; (left-fringe . 8)       ; half width left fringe width (def: 8)
                ;; (right-fringe . 0)      ; effectively disable right fringe
                ))

;; -----------------------------------------
;; notes for when you forget
;; -----------------------------------------
; (from source code)
; M-. : jump to function definition (xref-find-definitions)
; M-, : return to last position     (xref-pop-marker-stack)
; M-? : jump to function invocation (xref-find-references)

; (in xref buffer)
; C-o : open file in new pane, but stay in xref buffer (xref-show-location-at-point)

; (helpful commands)
; list-faces-display : show current font face definition (color / style)
; list-colors-display : show displayable colors

; (misc)
; M-= : display word / character count
