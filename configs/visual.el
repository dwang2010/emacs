;; ------------------------------------------------------------------------
;; visual theme
;; ------------------------------------------------------------------------
;; https://github.com/purcell/color-theme-sanityinc-tomorrow
;; options: day, night, blue, bright, eighties
(use-package color-theme-sanityinc-tomorrow :ensure t :defer)

;; https://protesilaos.com/emacs/ef-themes-pictures
;; light: ef-light / ef-melissa-light
;; dark: ef-elea-dark / ef-melissa-dark
(use-package ef-themes :ensure t :defer)

;; https://github.com/nordtheme/emacs
;; has some issues running as daemon / cliente
(use-package nord-theme :ensure t :defer)

;; change theme based on time of day
;; https://github.com/guidoschmidt/circadian.el
(use-package circadian
  :ensure t
  :config
  (setq-default circadian-themes '(("7:00" . ef-melissa-dark)
                                   ("18:30" . ef-elea-dark)))
  (circadian-setup))

;; ------------------------------------------------------------------------
;; mode line customization
;; ------------------------------------------------------------------------
(use-package mood-line
  :ensure t
  :config (mood-line-mode)
  (setq-default mood-line-glyph-alist mood-line-glyphs-ascii))

;; ------------------------------------------------------------------------
;; display customization
;; ------------------------------------------------------------------------
;; default font face, based on OS
(if (eql system-type 'darwin)
    (progn
      (set-face-attribute 'default nil :height 150)
      (set-face-attribute 'fixed-pitch nil :family "Menlo" :height 1.0)
      (setq-default line-spacing nil))
  (progn
    (set-face-attribute 'default nil :height 125)
    (set-face-attribute 'fixed-pitch nil :family "Ubuntu Mono" :height 1.0)
    (setq-default line-spacing nil)))

;; default frame parameters
(setq-default default-frame-alist
              '((width . 100)              ; window width (chars, less line numbers)
                (height . 55)              ; window height (rows)
                (cursor-type . bar)        ; vertical bar cursor
                (cursor-color . "#ff7f00") ; cursor color
                ;; (left-fringe . 8)       ; half width left fringe width (def: 8)
                ;; (right-fringe . 0)      ; effectively disable right fringe
                ))

;; ensure scrollbars don't appear when creating new frames
(defun rmv-scroll-bars (frame)
  (modify-frame-parameters frame
                           '((vertical-scroll-bars . nil)
                             (horizontal-scroll-bars . nil))))
(add-hook 'after-make-frame-functions 'rmv-scroll-bars)
