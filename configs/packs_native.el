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

;; custom project root detection
;; ensures lsp-mode starts hh_client in www/ not fbsource/
;; and flow lsp in xplat/js/ not fbsource/
(defun my-hack-project-root (dir)
  "Return nearest .hhconfig, .flowconfig, or .git root as project root."
  (or (when-let ((root (locate-dominating-file dir ".hhconfig")))
        (cons 'transient root))
      (when-let ((root (locate-dominating-file dir ".flowconfig")))
        (cons 'transient root))
      (when-let ((root (locate-dominating-file dir ".git")))
        (cons 'transient root))))
(add-hook 'project-find-functions #'my-hack-project-root -10)

;; ------------------------------------------------------------------------
;; tree sitter - syntax highlighting (native in emacs >= 29)
;; ------------------------------------------------------------------------
(use-package treesit
  :preface
  (defun my/setup-treesit-grammars ()
    "Install Tree-sitter grammars if they are absent."
    (interactive)
    (dolist (grammar
             '((javascript . ("https://github.com/tree-sitter/tree-sitter-javascript"))
               (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src"))
               (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src"))
               (python . ("https://github.com/tree-sitter/tree-sitter-python"))
               (bash . ("https://github.com/tree-sitter/tree-sitter-bash"))
               (json . ("https://github.com/tree-sitter/tree-sitter-json"))))
      (add-to-list 'treesit-language-source-alist grammar)
      (unless (treesit-language-available-p (car grammar))
        (treesit-install-language-grammar (car grammar)))))

  :config
  (my/setup-treesit-grammars)
  (setq-default treesit-font-lock-level 4)

  ;; remap traditional modes to tree-sitter counterparts
  (dolist (mapping
           '((python-mode . python-ts-mode)
             (js-mode . js-ts-mode)
             (js-jsx-mode . js-ts-mode)
             (js-json-mode . json-ts-mode)
             (sh-mode . bash-ts-mode)))
    (add-to-list 'major-mode-remap-alist mapping))

  ;; file types with no traditional mode to remap from
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.jsx\\'" . js-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode)))

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
;; tramp config
;; lsp-mode only possible if language server present on remote
;; try sshx if having issues with ssh
;; https://www.gnu.org/software/emacs/manual/html_node/tramp/Inline-methods.html
;; ------------------------------------------------------------------------
(setq-default tramp-default-method "rsync")
(setq-default tramp-terminal-type "tramp")

;; connecting via multiple hops
;; https://www.gnu.org/software/emacs/manual/html_node/tramp/Multi_002dhops.html

;; for debugging
;; (setq-default tramp-default-user "")
;; (setq-default tramp-default-host "")
;; (setq-default tramp-chunksize 500)
;; (setq-default tramp-verbose 10)

;; check tramp manual 5.6.9 on performance

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
  :bind (("<C-tab>" . hs-toggle-hiding)    ; GUI
         ("<C-M-tab>" . 'hs-hide-all)      ; GUI
         ("C-c h h" . hs-toggle-hiding)    ; terminal-safe
         ("C-c h a" . hs-hide-all)         ; terminal-safe
         ("C-c h s" . hs-show-all))        ; terminal-safe
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

;; ------------------------------------------------------------------------
;; calendar
;; . : goto today
;; < : shift month left
;; > : shift month right
;; ------------------------------------------------------------------------
(use-package calendar
  :bind ("C-c d" . 'calendar)
  :config
  ;; mark today's date on calendar
  (set-face-attribute 'calendar-today nil :underline nil :foreground "#cd5555")
  (add-hook 'calendar-today-visible-hook 'calendar-mark-today)

  ;; config calendar window popup behavior
  (add-to-list 'display-buffer-alist
               '("*Calendar*"
                 (display-buffer-reuse-window display-buffer-below-selected))))
