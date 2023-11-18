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
