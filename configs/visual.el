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
;; (use-package doom-themes
;;   :ensure t
;;   :config
;;   ;; doom vibrant configs
;;   (load-theme 'doom-vibrant t)
;;   (set-face-attribute 'show-paren-match nil :bold t))

;; https://github.com/catppuccin/emacs
;; descending dark: mocha / macchiato / frappe
;; light: latte
(use-package catppuccin-theme
  :ensure t
  :config
  (load-theme 'catppuccin :no-confirm)
  (setq-default catppuccin-flavor 'macchiato)
  (setq-default catppuccin-enlarge-headings nil)
  (setq-default catppuccin-italic-blockquotes nil)
  (catppuccin-reload)
  ;; increase contrast of selected buffer
  (set-face-attribute 'mode-line-active nil :background "#1e2030")
  (set-face-attribute 'mode-line-inactive nil :background "#363a4f"))

(set-cursor-color "#ff7f00") ; needed for init reload
(set-face-attribute 'font-lock-doc-face nil :foreground "#8a8a93")
(set-face-attribute 'font-lock-comment-face nil :foreground "#81868b")

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
;; pulsar - transiently highlight current line
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Defining-Faces.html
;; ------------------------------------------------------------------------
(defface my-pulsar-color
  '((t :extend t :inverse-video t :background "#cd6600"))
  "custom pulsar face - note changes require restarting emacs")

(use-package pulsar
  :ensure t
  :custom
  (pulsar-face 'my-pulsar-color)
  (pulsar-delay 0.03) ; fade out time
  (pulsar-pulse-functions ; functions to trigger on
   '(recenter
     windmove-right
     windmove-left
     windmove-up
     windmove-down
     org-next-visible-heading
     org-previous-visible-heading
     org-forward-heading-same-level
     org-backward-heading-same-level
     ace-window))
  :config
  (pulsar-global-mode 1))

;; ------------------------------------------------------------------------
;; highlight current line - only in specific modes (native)
;; ------------------------------------------------------------------------
(use-package hl-line
  :hook ((prog-mode . hl-line-mode)
         (dired-mode . hl-line-mode)))

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
;; default font + height, OS dependent
;; uses different face on mode-line for visual contrast
(if (eql system-type 'darwin)
    (progn
      (set-face-attribute 'default nil :family "Menlo" :height 150)
      (set-face-attribute 'fixed-pitch nil :family "Menlo" :height 1.0)
      (set-face-attribute `mode-line-active nil :family "Jetbrains Mono" :height 150)
      (set-face-attribute `mode-line-inactive nil :family "Jetbrains Mono" :height 150)
      (setq-default line-spacing nil))
  (progn
    (set-face-attribute 'default nil :family "Hack" :height 100)
    (set-face-attribute 'fixed-pitch nil :family "Hack" :height 1.0)
    (set-face-attribute `mode-line-active nil :family "Jetbrains Mono" :height 105)
    (set-face-attribute `mode-line-inactive nil :family "Jetbrains Mono" :height 105)
    (setq-default line-spacing nil)))

;; default frame parameters
(setq-default default-frame-alist
              '((width . 100) ; window width (cols)
                (height . 55) ; window height (rows)
                (cursor-color . "#ff7f00"))) ; needed for emacsclient startup

;; ensure scrollbars don't appear when creating new frames
(defun rmv-scroll-bars (frame)
  (modify-frame-parameters frame
                           '((vertical-scroll-bars . nil)
                             (horizontal-scroll-bars . nil))))
(add-hook 'after-make-frame-functions 'rmv-scroll-bars)

;; frame chrome
(if (eql system-type 'darwin)
    ;; dark themed title bar
    (add-to-list 'default-frame-alist '(ns-appearance . dark)))
