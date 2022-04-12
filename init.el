(package-initialize)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(when (and (equal emacs-version "27.2")
           (eql system-type 'darwin))
  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

;; reduce number of garbage collections to reduce startup time
(setq gc-cons-threshold 50000000) ; 50MB

;; ------------------------------------------------------------------------
;; custom set configuration
;; ------------------------------------------------------------------------
(custom-set-variables
 '(custom-safe-themes
   '("628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" default))
 '(package-selected-packages
   '(go-mode yaml-mode company emojify avy dumb-jump yasnippet python magit org)))

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
(setq-default fill-column 80)

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

;; adjust mouse settings
(setq-default mouse-wheel-progressive-speed nil)

;; increase xref highlight fade delay time
(setq-default pulse-delay 0.09)

;; exit confirmation (since slippery fingers sometimes)
(setq-default confirm-kill-emacs 'y-or-n-p)

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

;; toggle flyspell for spellchecking (assuming ispell / aspell installed)
(global-set-key (kbd "C-c C-4") 'flyspell-mode)

;; bind ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; magit status
(global-set-key (kbd "C-x g") 'magit-status)

;; unbind hotkeys to suspend frame
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))
(global-unset-key (kbd "C-x C-d"))

;; change C-x s to same as C-x C-s
(global-set-key (kbd "C-x s") 'save-buffer)

;; logical switching between visible window panes
(global-set-key (kbd "<C-M-left>") 'windmove-left)
(global-set-key (kbd "<C-M-right>") 'windmove-right)
(global-set-key (kbd "<C-M-up>") 'windmove-up)
(global-set-key (kbd "<C-M-down>") 'windmove-down)

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
;; additional control for displaying buffers
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Choosing-Window-Options.html
;; ------------------------------------------------------------------------
;(setq-default split-height-threshold nil) ; new window below
;(setq-default split-width-threshold 80)  ; new window to right

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
(global-set-key (kbd "C-f") 'avy-goto-char-2)
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

(require 'ls-lisp)
(setq-default ls-lisp-use-insert-directory-program nil)
(setq-default ls-lisp-dirs-first t)

;; ------------------------------------------------------------------------
;; dumbjump settings - jump to definition!
;; ------------------------------------------------------------------------
(add-hook 'xref-backend-functions #'dumb-jump-xref-activate)

;; ------------------------------------------------------------------------
;; emojify settings
;; ------------------------------------------------------------------------
;(add-hook 'after-init-hook #'global-emojify-mode)

;; ------------------------------------------------------------------------
;; flyspell settings
;; ------------------------------------------------------------------------
(setq-default flyspell-issue-message-flag nil)

;; ------------------------------------------------------------------------
;; hideshow settings - code folding!
;; ------------------------------------------------------------------------
(add-hook 'prog-mode-hook #'hs-minor-mode)

(global-set-key (kbd "<C-tab>") 'hs-toggle-hiding)
(global-set-key (kbd "<C-M-tab>") 'hs-hide-all)

;; ------------------------------------------------------------------------
;; js-mode settings
;; ------------------------------------------------------------------------
(setq js-indent-level 2)

;; ------------------------------------------------------------------------
;; magit related
;; ------------------------------------------------------------------------
(setq-default magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1)

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
;; python related settings
;; ------------------------------------------------------------------------
(if (eql system-type 'darwin)
    (setq python-shell-interpreter "/usr/local/bin/python3")
    (setq python-shell-interpreter "/usr/bin/python3"))

;; ------------------------------------------------------------------------
;; verilog-mode settings
;; (https://www.veripool.org/projects/verilog-mode/wiki/Verilog-mode-Help)
;; ------------------------------------------------------------------------
(setq-default verilog-align-ifelse nil)
(setq-default verilog-auto-indent-on-newline t)
(setq-default verilog-auto-lineup (quote all))
(setq-default verilog-auto-newline nil)
(setq-default verilog-case-indent 3)
(setq-default verilog-cexp-indent 3)
(setq-default verilog-indent-begin-after-if nil)
(setq-default verilog-indent-level 3)
(setq-default verilog-indent-level-behavioral 3)
(setq-default verilog-indent-level-declaration 3)
(setq-default verilog-indent-level-directive 0)
(setq-default verilog-indent-level-module 3)
(setq-default verilog-indent-lists t)
(setq-default verilog-minimum-comment-distance 20)

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

;; non-terminal display customization
(if (display-graphic-p)
    (progn
      (load-theme 'sanityinc-tomorrow-eighties)
      (setq default-frame-alist ; change default frame parameters
            '(
              (width . 85)               ; window width (chars, less line numbers)
              (height . 50)              ; window height (rows)
              (cursor-type . bar)        ; vertical bar cursor
              (cursor-color . "#ff7f00") ; orange cursor color
              (left-fringe . 6)          ; half width left fringe width (def: 8)
              (right-fringe . 0)         ; effectively disable right fringe
              )
            )
      )
  ;; terminal specific customization (if desired below)
  )

;; startup time init display, revert to more frequent garbage collection
(add-hook 'emacs-startup-hook 'my-startup-chk)
(defun my-startup-chk ()
  (setq gc-cons-threshold 800000) ; 800kB
  (message "emacs startup in %s."
           (format "%.2f seconds"
                   (float-time (time-subtract after-init-time before-init-time)))))
(custom-set-faces
 '(avy-lead-face ((t (:background "red2" :foreground "white"))))
 '(avy-lead-face-0 ((t (:background "saddle brown" :foreground "white"))))
 '(avy-lead-face-1 ((t (:background "#008b8b" :foreground "black"))))
 '(avy-lead-face-2 ((t (:background "saddle brown" :foreground "white"))))
 '(show-paren-match ((t (:background "#00ffff" :foreground "#2d2d2d")))))

;; -----------------------------------------
;; notes for when you forget
;; -----------------------------------------
; (from source code)
; M-. : jump to function definition (xref-find-definitions)
; M-, : return to last position     (xref-pop-marker-stack)
; M-? : jump to function invocation (xref-find-references)

; (in xref buffer)
; C-o : open file in new pane, but stay in xref buffer (xref-show-location-at-point)

; (general searching)
; C-s C-w : search for word after current mark
; C-w     : subsequent presses increases search with additional words

; (helpful commands)
; list-faces-display : show current font face definition (color / style)
; list-colors-display : show displayable colors
