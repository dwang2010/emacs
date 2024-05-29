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
  (put 'narrow-to-region 'disabled nil) ; allow narrow-to-region
  (global-so-long-mode 1) ; avoid performance issues in long line files
  (global-eldoc-mode -1) ; disable eldoc mode globally
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
  (keymap-global-unset "s-q") ; save-buffers-kill-emacs

  :bind
  (("C-x C-b" . ibuffer) ; bind ibuffer
   ("C-x k" . kill-this-buffer) ; just kill buffer, don't ask
   ("C-x s" . save-buffer) ; redundant save for lazy fingers
   ("C-c a" . align-regexp)
   ("C-c s" . replace-string)
   ("C-l" . recenter-top-bottom)
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
  (cursor-type 'bar) ; set default cursor type to vertical bar

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
  (gc-cons-threshold 1600000) ; too high, memory hog; too small, some slowdown
  (read-process-output-max (* 1024 1024)) ; increase data read chunk size
  (bidi-display-reordering 'left-to-right) ; disable bidirectional text scan
  (bidi-paragraph-direction 'left-to-right) ; text should only go one direction
  (bidi-inhibit-bpa t) ; not using bidirectional text, so disable this
  (frame-inhibit-implied-resize t) ; don't try to preserve rows / cols

  ;; mouse behavior variables
  (mouse-wheel-progressive-speed nil))

;; system specific customizations
(if (eql system-type 'darwin)
    (progn
      (setq-default mac-option-modifier 'meta)
      (setq-default mac-command-modifier 'super))
  (progn
    (setq-default mouse-wheel-scroll-amount '(5))))
(global-set-key (kbd "s-f") 'toggle-frame-fullscreen)
(global-set-key (kbd "s-m") 'toggle-frame-maximized)

(defun revert-buffer-no-confirm ()
  "Revert buffer without confirmation"
  (interactive) (revert-buffer t t))

(defun reload-init-files ()
  "Reload all init files"
  (interactive) (load-file user-init-file))

(defun edit-current-file-as-root ()
  "Edit the file that is associated with the current buffer as root"
  (interactive)
  (let ((filep (buffer-file-name)))
    (if filep (find-file (concat "/sudo::" filep))
      (message "Current buffer does not have an associated file."))))
