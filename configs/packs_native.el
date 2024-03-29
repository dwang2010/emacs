;; ------------------------------------------------------------------------
;; project based file management (native)
;; ------------------------------------------------------------------------
(use-package project ; test native
  ;; native binds
  ;; C-x p d == (dired to target dir)
  ;; C-x p D == (dired to root dir)
  :config
  ;; always just find file when switching between projects
  (setq-default project-switch-commands 'project-find-file))

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
;; winner mode - undo / redo window config changes (native)
;; C-c left (undo) / C-c right (redo)
;; ------------------------------------------------------------------------
(use-package winner
  :config
  (winner-mode 1))

;; ------------------------------------------------------------------------
;; paren - highlight matching braces (native)
;; ------------------------------------------------------------------------
(use-package paren
  :config (show-paren-mode 1)
  :custom
  (show-paren-context-when-offscreen 'overlay) ; popup paren context @ top left (v29.1+)
  (show-paren-delay 0))

;; ------------------------------------------------------------------------
;; hideshow - code folding (native)
;; ------------------------------------------------------------------------
(use-package hideshow
  :bind (("<C-tab>" . hs-toggle-hiding)
         ("<C-M-tab>" . 'hs-hide-all))
  :hook ((prog-mode . hs-minor-mode)
         (c-mode-common . hs-minor-mode))
  :custom
  (hs-hide-comments-when-hiding-all nil))

;; ------------------------------------------------------------------------
;; flyspell - spell checking (native)
;; ispell needs to be locally installed (not a part of emacs)
;; ------------------------------------------------------------------------
(use-package flyspell
  :bind ("C-c C-4" . 'flyspell-mode)
  :config (setq-default flyspell-issue-message-flag nil))

;; M-$ = Check and correct spelling of the word at point
;; flyspell-buffer to check spelling of entire buffer
