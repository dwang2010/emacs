(package-initialize)
(add-to-list 'package-archives '("melpa-stable" . "https://melpa.org/packages/") t)

;; reduce number of garbage collections to reduce startup time
(setq gc-cons-threshold 50000000) ; 50MB

;; ------------------------------------------------------------------------
;; custom set configuration
;; ------------------------------------------------------------------------
(custom-set-variables
 '(custom-safe-themes
   (quote
    ("628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" default)))
 '(package-selected-packages (quote (python iy-go-to-char magit ahk-mode org))))

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

;; iy-go-to-char
(global-set-key (kbd "C-c f") 'iy-go-to-char)
(global-set-key (kbd "C-c b") 'iy-go-to-char-backward)

;; ------------------------------------------------------------------------
;; magit settings
;; ------------------------------------------------------------------------
(setq magit-refresh-status-buffer nil)

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
;; python related settings
;; ------------------------------------------------------------------------
(setq python-shell-interpreter "/usr/bin/python3")

;; ------------------------------------------------------------------------
;; display customization
;; ------------------------------------------------------------------------
;; theme locations
(add-to-list 'load-path "~/.emacs.d/themes/")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

;; default font face
(set-face-attribute 'default nil :family "Consolas" :height 100)
(setq-default line-spacing 0.1)

;; non-terminal display customization
(if (display-graphic-p)
    (progn
      (load-theme 'sanityinc-tomorrow-eighties)
      (setq default-frame-alist ; change default frame parameters
            '(
              (width . 105)              ; window width (chars, less line numbers)
              (height . 55)              ; window height (rows)
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
