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
   '(company highlight-indent-guides emojify avy dumb-jump yasnippet python magit org)))

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

;; mouse settings
(setq-default mouse-wheel-progressive-speed nil)

;; increase xref highlight fade delay time
(setq-default pulse-delay 0.09)

;; exit confirmation (since slippery fingers sometimes)
(setq-default confirm-kill-emacs 'y-or-n-p)

;; ensure scrollbars don't appear when creating new frames
(defun rmv-scroll-bars (frame)
  (modify-frame-parameters frame
                           '((vertical-scroll-bars . nil)
                             (horizontal-scroll-bars . nil))))
(add-hook 'after-make-frame-functions 'rmv-scroll-bars)

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
;; avy settings (jump to char!)
;; ------------------------------------------------------------------------
(global-set-key (kbd "C-f") 'avy-goto-char)
(setq-default avy-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
(setq-default avy-style 'de-bruijn)
(setq-default avy-background nil)

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
;; paren mode - highlight matching braces!
;; ------------------------------------------------------------------------
(show-paren-mode 1)
(setq-default show-paren-delay 0)

;; ------------------------------------------------------------------------
;; yasnippet settings - use default folder ~/.emacs.d/snippets
;; ------------------------------------------------------------------------
;(yas-global-mode 1)

;; ------------------------------------------------------------------------
;; dumbjump settings - jump to definition!
;; ------------------------------------------------------------------------
(add-hook 'xref-backend-functions #'dumb-jump-xref-activate)

;; ------------------------------------------------------------------------
;; emojify settings
;; ------------------------------------------------------------------------
(add-hook 'after-init-hook #'global-emojify-mode)

;; ------------------------------------------------------------------------
;; highlight indentation / indent guides settings
;; ------------------------------------------------------------------------
(add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
(setq-default highlight-indent-guides-method 'column)

;(with-eval-after-load 'highlight-indentation
;  (set-face-background 'highlight-indentation-face "#c3b3b3"))

;; ------------------------------------------------------------------------
;; hideshow settings - code folding!
;; ------------------------------------------------------------------------
(add-hook 'prog-mode-hook #'hs-minor-mode)

(global-set-key (kbd "<C-tab>") 'hs-toggle-hiding)
(global-set-key (kbd "<C-M-tab>") 'hs-hide-all)

;; ------------------------------------------------------------------------
;; company mode - complete anything!
;; ------------------------------------------------------------------------
(add-hook 'prog-mode-hook 'company-mode)

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
;; display customization
;; ------------------------------------------------------------------------
;; theme locations
(add-to-list 'load-path "~/.emacs.d/themes/")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

;; default font face, based on OS
(if (eql system-type 'darwin)
    (set-face-attribute 'default nil :family "Consolas" :height 125)
    (set-face-attribute 'default nil :family "Consolas" :height 100))
(setq-default line-spacing 0.1)

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
