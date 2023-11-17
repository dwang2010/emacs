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
(setq-default fill-column 85)

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

;; reduce frequency of garbage collection; happen on 50MB of allocated data
(setq-default gc-cons-threshold 50000000) ;; 50MB

;; increase data which emacs reads (helpful for lsp server)
(setq-default read-process-output-max (* 1024 1024)) ;; 1mb

;; goodbye to custom-set-variable mutations (just don't load)
(setq-default custom-file "~/.emacs.d/custom.el")

;; ------------------------------------------------------------------------
;; mouse behavior
;; ------------------------------------------------------------------------
(setq-default mouse-wheel-progressive-speed nil)
(if (not (eql system-type 'darwin)) (setq-default mouse-wheel-scroll-amount '(5)))

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

;; bind revert-buffer to F5 ("refresh") -- note, no confirmation message!
(defun revert-buffer-no-confirm ()
  "Revert buffer without confirmation."
  (interactive) (revert-buffer t t))

(global-set-key (kbd "<f5>") 'revert-buffer-no-confirm)

;; bind eval-buffer to F12
(global-set-key (kbd "<f12>") 'eval-buffer)

;; mac specific keybinds
(if (eql system-type 'darwin)
    (progn
      (setq-default mac-option-modifier 'meta)
      (setq-default mac-command-modifier 'hyper)
      (global-set-key [(hyper f)] 'toggle-frame-fullscreen)
      (global-set-key [(hyper m)] 'toggle-frame-maximized)))
