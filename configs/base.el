;; ------------------------------------------------------------------------
;; base emacs configuration, use-package style
;; ------------------------------------------------------------------------
(use-package emacs
  :hook
  (before-save . delete-trailing-whitespace) ; remove trailing whitespace

  :config
  (menu-bar-mode -1) ; disable menu bar
  (tool-bar-mode -1) ; disable tool bar
  (scroll-bar-mode -1) ; disable vertical scrollbars
  (blink-cursor-mode 0) ; don't blink cursor
  (delete-selection-mode t) ; overwrite currently selected region
  (prefer-coding-system 'utf-8) ; default coding system
  (global-display-line-numbers-mode) ; enable line numbers
  (put 'downcase-region 'disabled nil) ; allow region lowercase
  (put 'upcase-region 'disabled nil) ; allow region uppercase
  (global-so-long-mode 1) ; avoid performance issues in long line files
  (defalias 'yes-or-no-p 'y-or-n-p) ; simple (y or n) on prompts

  ;; key un-bindings
  (keymap-global-unset "C-z") ; suspend frame
  (keymap-global-unset "C-x C-z") ; suspend frame
  (keymap-global-unset "C-x k") ; kill buffer
  (keymap-global-unset "C-x C-d") ; list dirs
  (keymap-global-unset "C-x C-c") ; close emacs
  (keymap-global-unset "C-x C-r") ; find file read only
  (keymap-global-unset "C-<wheel-up>") ; scroll wheel zoom
  (keymap-global-unset "C-<wheel-down>") ; scroll wheel zoom

  :bind
  (("C-x C-b" . ibuffer) ; bind ibuffer
   ("C-x k" . kill-this-buffer) ; just kill buffer, don't ask
   ("C-x s" . save-buffer) ; redundant save for lazy fingers
   ("C-c a" . align-regexp)
   ("C-c s" . replace-string)
   ("C-l" . recenter)
   ("<f5>" . revert-buffer-no-confirm)
   ("<f12>" . reload-init-files)
   ("<C-M-left>" . windmove-left)
   ("<C-M-right>" . windmove-right)
   ("<C-M-up>" . windmove-up)
   ("<C-M-down>" . windmove-down)
   ("<home>" . move-beginning-of-line)
   ("<end>" . move-end-of-line))

  :custom
  ;; UI config variables
  (frame-title-format "%b") ; show filename in title bar
  (frame-resize-pixelwise t) ; remove window retiling gaps
  (inhibit-startup-screen t) ; remove startup screen
  (initial-scratch-message "") ; startup scratch message
  (initial-major-mode 'text-mode) ; startup major mode
  (ring-bell-function 'ignore) ; disable visible bell flashing
  (use-dialog-box nil) ; prompts to minibuffer instead of GUI
  (visible-bell 1) ; remove emacs bell noise
  (column-number-mode t) ; enable column numbers
  (help-window-select t) ; switch to help buffers automatically
  (completions-format 'vertical) ; sort completions minibuffer vertically
  (ibuffer-default-sorting-mode 'filename/process) ; ibuffer sort by filename

  ;; formatting config variables
  (indent-tabs-mode nil) ; spaces instead of tabs when indenting
  (kill-whole-line t) ; remove newline as well if at start of line
  (truncate-lines t) ; visually truncate lines (meaning don't wrap)
  (fill-column 80) ; default fill paragraph width
  (tab-width 4) ; default number of spaces for tab
  (require-final-newline t) ; ensure newline at end of file
  (sentence-end-double-space nil) ; only one space after sentence

  ;; location variables
  (custom-file "~/.emacs.d/custom.el") ; bye custom-set-variable mutations
  (backup-directory-alist '(("." . "~/.emacs.d/backups"))) ; central backups

  ;; performance improvement variables
  (gc-cons-threshold 100000000) ; garbage collection threshold (100MB)
  (read-process-output-max (* 1024 1024)) ; increase data read chunk size
  (bidi-display-reordering 'left-to-right) ; disable bidirectional text scan
  (bidi-paragraph-direction 'left-to-right) ; text should only go one direction
  (bidi-inhibit-bpa t) ; not using bidirectional text, so disable this

  ;; mouse behavior variables
  (mouse-wheel-progressive-speed nil)

  ;; buffer display control
  (display-buffer-alist
   '(
     ;; ( BUFFER-MATCHER
     ;;   LIST-OF-DISPLAY-FUNCTIONS
     ;;   &optional PARAMETERS )
     ("\\(\\*Capture\\*\\|CAPTURE-.*\\|\\*Org Select\\*\\)"
      (display-buffer-same-window display-buffer-below-selected))))
  )

;; system specific customizations
(if (eql system-type 'darwin)
    (progn
      (setq-default mac-option-modifier 'meta)
      (setq-default mac-command-modifier 'hyper)
      (global-set-key [(hyper f)] 'toggle-frame-fullscreen)
      (global-set-key [(hyper m)] 'toggle-frame-maximized))
  (progn
    (setq-default mouse-wheel-scroll-amount '(5))
    (global-set-key (kbd "s-f") 'toggle-frame-fullscreen)
    (global-set-key (kbd "s-m") 'toggle-frame-maximized)))

;; revert buffer without confirmation
(defun revert-buffer-no-confirm ()
  (interactive) (revert-buffer t t))

;; reload all init files
(defun reload-init-files ()
  (interactive) (load-file user-init-file))
