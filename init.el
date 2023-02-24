(package-initialize)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(when (and (equal emacs-version "28.1")
           (eql system-type 'darwin))
  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

;; reduce number of garbage collections to reduce startup time
(setq gc-cons-threshold 200000000)  ; 200MB
(setq read-process-output-max (* 1024 1024))  ; 1mb

;; ------------------------------------------------------------------------
;; custom set configuration
;; ------------------------------------------------------------------------
(custom-set-variables
 '(custom-safe-themes
   '("628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" default))
 '(package-selected-packages
   '(projectile lsp-mode flycheck salt-mode markdown-mode tree-sitter-langs tree-sitter cpputils-cmake eldoc-cmake vterm swiper go-mode yaml-mode company avy dumb-jump yasnippet python magit org))
 '(swiper-faces
   '(swiper-match-face-2 swiper-match-face-2 swiper-match-face-2 swiper-match-face-2))
 '(warning-suppress-log-types '((comp))))

;; ------------------------------------------------------------------------
;; default behaviors
;; ------------------------------------------------------------------------
;; disable menu bar
(menu-bar-mode -1)

;; disable tool bar
(tool-bar-mode -1)

;; disable vertical scrollbars
(scroll-bar-mode -1)

;; remove trailing whitespace on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; default coding system
(prefer-coding-system 'utf-8)

;; enable line numbers
(global-linum-mode 1)

;; remove startup screen
(setq-default inhibit-startup-screen t)

;; enable column numbers
(setq-default column-number-mode t)

;; default fill mode width
(setq-default fill-column 100)

;; spaces instead of tabs when indenting
(setq-default indent-tabs-mode nil)

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

;; increase xref highlight fade delay time
(setq-default pulse-delay 0.09)

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

;; change C-x s to same as C-x C-s
(global-set-key (kbd "C-x s") 'save-buffer)

;; logical switching between visible window panes
(global-set-key (kbd "<C-M-left>") 'windmove-left)
(global-set-key (kbd "<C-M-right>") 'windmove-right)
(global-set-key (kbd "<C-M-up>") 'windmove-up)
(global-set-key (kbd "<C-M-down>") 'windmove-down)

;; end key to end of line rather than end of buffer
(global-set-key (kbd "<home>") 'move-beginning-of-line)
(global-set-key (kbd "<end>") 'move-end-of-line)

;; mac specific keybinds
(if (eql system-type 'darwin)
    (progn
      (setq-default mac-option-modifier 'meta)
      (setq-default mac-command-modifier 'hyper)
      (global-set-key [(hyper f)] 'toggle-frame-fullscreen)
      (global-set-key [(hyper m)] 'toggle-frame-maximized)
      )
  )

;; ------------------------------------------------------------------------
;; tree sitter syntax highlighting config
;; ------------------------------------------------------------------------
(global-tree-sitter-mode)
(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)

;; ------------------------------------------------------------------------
;; additional control for displaying buffers
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Choosing-Window-Options.html
;; ------------------------------------------------------------------------
;; Fix annoying vertical window splitting.
;; https://lists.gnu.org/archive/html/help-gnu-emacs/2015-08/msg00339.html
(with-eval-after-load "window"
  (defcustom split-window-below nil
    "If non-nil, vertical splits produce new windows below."
    :group 'windows
    :type 'boolean)

  (defcustom split-window-right nil
    "If non-nil, horizontal splits produce new windows to the right."
    :group 'windows
    :type 'boolean)

  (fmakunbound #'split-window-sensibly)

  (defun split-window-sensibly
      (&optional window)
    (setq window (or window (selected-window)))
    (or (and (window-splittable-p window t)
             ;; Split window horizontally.
             (split-window window nil (if split-window-right 'left  'right)))
        (and (window-splittable-p window)
             ;; Split window vertically.
             (split-window window nil (if split-window-below 'above 'below)))
        (and (eq window (frame-root-window (window-frame window)))
             (not (window-minibuffer-p window))
             ;; If WINDOW is the only window on its frame and is not the
             ;; minibuffer window, try to split it horizontally disregarding the
             ;; value of `split-width-threshold'.
             (let ((split-width-threshold 0))
               (when (window-splittable-p window t)
                 (split-window window nil (if split-window-right
                                              'left
                                            'right))))))))

(setq-default split-height-threshold  4
              split-width-threshold   160) ; the reasonable limit for horizontal splits

;; ------------------------------------------------------------------------
;; avy settings (jump to char!)
;; ------------------------------------------------------------------------
(global-set-key (kbd "C-f") 'avy-goto-char-timer)
(setq-default avy-timeout-seconds 0.20)
(setq-default avy-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
(setq-default avy-style 'de-bruijn)
(setq-default avy-background nil)

;; ------------------------------------------------------------------------
;; company mode - complete anything!
;; ------------------------------------------------------------------------
;(add-hook 'prog-mode-hook 'company-mode)

;; ------------------------------------------------------------------------
;; dired settings
;; ------------------------------------------------------------------------
(global-set-key (kbd "C-x C-d") 'dired)
(put 'dired-find-alternate-file 'disabled nil)
(setq-default dired-dwim-target t)
(setq-default dired-listing-switches "-laGh --group-directories-first -v")

(require 'ls-lisp)
(setq-default ls-lisp-use-insert-directory-program nil)
(setq-default ls-lisp-dirs-first t)

;; ------------------------------------------------------------------------
;; dumbjump settings - jump to definition!
;; ------------------------------------------------------------------------
;; (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)

;; ------------------------------------------------------------------------
;; flycheck settings
;; ------------------------------------------------------------------------
;; flycheck requires local installation of external programs to work
;; https://www.flycheck.org/en/latest/languages.html#python
(global-set-key (kbd "C-c ! !") 'flycheck-mode)

(add-hook 'python-mode-hook 'flycheck-mode)
(add-hook 'go-mode-hook 'flycheck-mode)

(setq-default flycheck-relevant-error-other-file-show nil)
(setq-default flycheck-check-syntax-automatically '(mode-enabled save))

;; handy shortcuts:
;; C-c ! v = verify setup
;; C-c ! l = list of errors in buffer

;; ------------------------------------------------------------------------
;; flyspell settings
;; ------------------------------------------------------------------------
;; ispell needs to be locally installed (not a part of emacs)
(setq-default flyspell-issue-message-flag nil)
(global-set-key (kbd "C-c C-4") 'flyspell-mode)

;; ------------------------------------------------------------------------
;; hideshow settings - code folding!
;; ------------------------------------------------------------------------
(add-hook 'prog-mode-hook #'hs-minor-mode)
(global-set-key (kbd "<C-tab>") 'hs-toggle-hiding)
(global-set-key (kbd "<C-M-tab>") 'hs-hide-all)

;; ------------------------------------------------------------------------
;; golang related settings
;; ------------------------------------------------------------------------
(setq-default tab-width 4)
(if (eql system-type 'darwin) (add-hook 'before-save-hook 'gofmt-before-save))

; env related workaround
(if (eql system-type 'darwin)
    (progn
      (setenv "PATH" (concat (getenv "PATH") ":/Users/darrenw/go/bin"))
      (setq-default exec-path (append exec-path '("/Users/darrenw/go/bin")))
      )
  )

;; ------------------------------------------------------------------------
;; js-mode settings
;; ------------------------------------------------------------------------
(setq js-indent-level 4)

;; ------------------------------------------------------------------------
;; emacs lsp related
;; ------------------------------------------------------------------------
(setq-default lsp-keymap-prefix "C-c l")

;; (setq-default lsp-enable-symbol-highlighting nil)
;; (setq-default lsp-headerline-breadcrumb-enable nil)

(setq-default lsp-log-io nil) ; if set to true can cause a performance hit
(setq-default lsp-lens-enable nil)

(setq-default lsp-ui-doc-enable nil)
(setq-default lsp-ui-sideline-enable nil)
(setq-default lsp-modeline-code-actions-enable nil)
(setq-default lsp-modeline-diagnostics-enable nil)
(setq-default lsp-modeline-workspace-status-enable nil)
(setq-default lsp-signature-render-documentation nil)
(setq-default lsp-diagnostics-provider :none)
(setq-default lsp-completion-provider :none)
(setq-default lsp-completion-show-detail nil)
(setq-default lsp-completion-show-kind nil)
(setq-default lsp-completion-enable nil)
(setq-default lsp-headerline-breadcrumb-enable-diagnostics nil)

;; lsp-mode doesn't seem to respect flycheck-check-syntax-automatically
;; linting continually occurs while typing, and produces distracting visual artifacts
(setq-default lsp-pylsp-plugins-flake8-enabled nil)
(setq-default lsp-pylsp-plugins-mccabe-enabled nil)
(if (eql system-type 'darwin) (setq-default lsp-pyls-server-command "/opt/homebrew/bin/pylsp"))

(require 'lsp-mode)
(add-hook 'python-mode-hook #'lsp)
(add-hook 'go-mode-hook #'lsp)

(global-set-key (kbd "M-.") 'lsp-find-definition)
(global-set-key (kbd "M-?") 'lsp-find-references)

;; disable lsp-mode right click menu
(with-eval-after-load 'lsp-mode
  (define-key lsp-mode-map (kbd "<mouse-3>") nil))

;; ------------------------------------------------------------------------
;; projectile related
;; ------------------------------------------------------------------------
(projectile-mode +1)
(global-set-key (kbd "C-x p") 'projectile-command-map)

;; ------------------------------------------------------------------------
;; magit related
;; ------------------------------------------------------------------------
(setq-default magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1)
(global-set-key (kbd "C-x g") 'magit-status)

;; ------------------------------------------------------------------------
;; org-mode related settings
;; ------------------------------------------------------------------------
(setq-default org-hide-emphasis-markers t)

(setq org-emphasis-alist
  '(("*" (bold :foreground "Orange"))
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

;; configure python interpretter based on system
(if (eql system-type 'darwin)
    (setq python-shell-interpreter "/usr/local/bin/python3")
  (setq python-shell-interpreter "/usr/bin/python3"))

;; set org-babel for desired python executable
(setq org-babel-python-command "python3")

;; potentially risky: remove confirmation when executing code blocks
(setq-default org-confirm-babel-evaluate nil)

;; C-c C-o opens link in same buffer rather than new buffer
(setq org-link-frame-setup
   '((vm . vm-visit-folder-other-frame)
     (vm-imap . vm-visit-imap-folder-other-frame)
     (gnus . org-gnus-no-new-news)
     (file . find-file)
     (wl . wl-other-frame)))

;; ------------------------------------------------------------------------
;; paren mode - highlight matching braces!
;; ------------------------------------------------------------------------
(setq-default show-paren-delay 0)
(show-paren-mode 1)

;; ------------------------------------------------------------------------
;; protobuf related settings
;; ------------------------------------------------------------------------
; 4-space indent
(defconst my-protobuf-style
  '((c-basic-offset . 4)
    (indent-tabs-mode . nil)))

(add-hook 'protobuf-mode-hook
          (lambda () (c-add-style "my-style" my-protobuf-style t)))

; ensure hideshow minor mode for proto files
(add-hook 'c-mode-common-hook 'hs-minor-mode)

;; ------------------------------------------------------------------------
;; ivy / swiper settings
;; ------------------------------------------------------------------------
(ivy-mode 1)
(global-set-key (kbd "C-s") 'swiper-isearch)
(global-set-key (kbd "C-r") 'swiper-backward)
(global-set-key (kbd "C-c C-w") 'swiper-isearch-thing-at-point)
(setq-default ivy-on-del-error-function #'ignore)
(setq-default ivy-display-style 'fancy)
(setq-default ivy-height 4)

;; ------------------------------------------------------------------------
;; vterm configuration
;; https://github.com/akermu/emacs-libvterm
;; ------------------------------------------------------------------------
(setq-default vterm-copy-exclude-prompt t)

;; ------------------------------------------------------------------------
;; yasnippet settings - use default folder ~/.emacs.d/snippets
;; ------------------------------------------------------------------------
;(yas-global-mode 1)

;; ------------------------------------------------------------------------
;; display customization
;; ------------------------------------------------------------------------
;; theme locations
(add-to-list 'load-path "~/.emacs.d/themes/")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

;; default font face, based on OS
(if (eql system-type 'darwin)
    (progn
      (set-face-attribute 'default nil :family "Consolas" :height 125)
      (setq-default line-spacing 0.05))
  (progn
    (set-face-attribute 'default nil :family "Consolas" :height 115)
    (setq-default line-spacing 0.05)))

;; default frame parameters
(load-theme 'sanityinc-tomorrow-eighties)
(setq-default default-frame-alist
              '((width . 100)              ; window width (chars, less line numbers)
                (height . 50)              ; window height (rows)
                (cursor-type . bar)        ; vertical bar cursor
                (cursor-color . "#ff7f00") ; orange cursor color
                (left-fringe . 8)          ; half width left fringe width (def: 8)
                (right-fringe . 0)))       ; effectively disable right fringe

(custom-set-faces
 '(avy-lead-face ((t (:background "red2" :foreground "white"))))
 '(avy-lead-face-0 ((t (:background "saddle brown" :foreground "white"))))
 '(avy-lead-face-1 ((t (:background "#008b8b" :foreground "black"))))
 '(avy-lead-face-2 ((t (:background "saddle brown" :foreground "white"))))
 '(isearch ((t (:background "#515151" :foreground "#ffcc66" :inverse-video t))))
 '(lsp-face-highlight-read ((t (:inherit highlight :background "SlateBlue4" :foreground "gray90"))))
 '(lsp-face-highlight-write ((t (:inherit highlight :background "SlateBlue4" :foreground "gray90"))))
 '(match ((t (:background "#2d2d2d" :foreground "#6699cc" :inverse-video nil))))
 '(show-paren-match ((t (:background "#2d2d2d" :foreground "#00ffff"))))
 '(swiper-background-match-face-2 ((t (:inherit swiper-match-face-1))))
 '(swiper-line-face ((t (:underline nil)))))

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
