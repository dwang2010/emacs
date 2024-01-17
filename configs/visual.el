;; ------------------------------------------------------------------------
;; visual theme
;; ------------------------------------------------------------------------
;; https://github.com/purcell/color-theme-sanityinc-tomorrow
;; options: day, night, blue, bright, eighties
;; (use-package color-theme-sanityinc-tomorrow :ensure t)

;; https://protesilaos.com/emacs/ef-themes-pictures
;; light: ef-light / ef-melissa-light
;; dark: ef-elea-dark / ef-melissa-dark
; (use-package ef-themes :ensure t)

;; https://github.com/nordtheme/emacs
;; has some issues running as daemon / client
;; (use-package nord-theme :ensure t)

;; https://github.com/doomemacs/themes
;; doom-vibrant / doom-one
(use-package doom-themes
  :ensure t
  :config
  ;; doom vibrant configs
  (load-theme 'doom-vibrant t)
  (set-cursor-color "#ff7f00")
  (set-face-attribute 'show-paren-match nil :bold t)
  (set-face-attribute 'font-lock-doc-face nil :foreground "#8a8a93")
  (set-face-attribute 'font-lock-comment-face nil :foreground "#81868b"))

;; ------------------------------------------------------------------------
;; visual icons
;; ------------------------------------------------------------------------
;; if rendering incorrectly invoke (nerd-icons-install-fonts)
(use-package nerd-icons
  :ensure t)

(use-package nerd-icons-dired
  :ensure t
  :hook (dired-mode . nerd-icons-dired-mode))

(use-package nerd-icons-completion
  :ensure t
  :after marginalia
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

(use-package nerd-icons-ibuffer
  :ensure t
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

;; ------------------------------------------------------------------------
;; mode line customization
;; ------------------------------------------------------------------------
(use-package mood-line
  :ensure t
  :config (mood-line-mode)
  (setq-default mood-line-show-encoding-information t)
  (setq-default mood-line-show-eol-style t)
  ;; glyph style: ascii / fira-code (get the font)
  (setq-default mood-line-glyph-alist mood-line-glyphs-fira-code))

;; ------------------------------------------------------------------------
;; display customization
;; ------------------------------------------------------------------------
;; default font face, based on OS
(if (eql system-type 'darwin)
    (progn
      (set-face-attribute 'default nil :family "Menlo" :height 150)
      (set-face-attribute 'fixed-pitch nil :family "Menlo" :height 1.0)
      ;; different face on mode-line to increase clarity
      (set-face-attribute `mode-line nil :family "Jetbrains Mono" :height 150)
      (set-face-attribute `mode-line-inactive nil :family "Jetbrains Mono" :height 150)
      (setq-default line-spacing nil))
  (progn
    ;; ubuntu mono for increased vertical density
    (set-face-attribute 'default nil :family "Ubuntu Mono" :height 120)
    (set-face-attribute 'fixed-pitch nil :family "Ubuntu Mono" :height 1.0)
    ;; different face on mode-line to increase clarity
    (set-face-attribute `mode-line nil :family "Jetbrains Mono" :height 105)
    (set-face-attribute `mode-line-inactive nil :family "Jetbrains Mono" :height 105)
    (setq-default line-spacing nil)))

;; default frame parameters
(setq-default default-frame-alist
              '((width . 100)                ; window width (cols)
                (height . 55)                ; window height (rows)
                (cursor-type . bar)          ; vertical bar cursor
                (cursor-color . "#ff7f00"))) ; cursor color

;; ensure scrollbars don't appear when creating new frames
(defun rmv-scroll-bars (frame)
  (modify-frame-parameters frame
                           '((vertical-scroll-bars . nil)
                             (horizontal-scroll-bars . nil))))
(add-hook 'after-make-frame-functions 'rmv-scroll-bars)
