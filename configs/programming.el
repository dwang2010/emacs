;; ------------------------------------------------------------------------
;; python configs
;; ------------------------------------------------------------------------
;; configure location of python interpreter (system dependent)
(if (eql system-type 'darwin)
    (setq-default python-shell-interpreter "/opt/homebrew/bin/python3.10")
  (setq-default python-shell-interpreter "/usr/bin/python3"))

;; single indent for multi-line function signature
(setq-default python-indent-def-block-scale 1)

;; ------------------------------------------------------------------------
;; golang configs
;; ------------------------------------------------------------------------
;(if (eql system-type 'darwin) (add-hook 'before-save-hook 'gofmt-before-save))

; env related workaround
;; (if (eql system-type 'darwin)
;;     (progn
;;       (setenv "PATH" (concat (getenv "PATH") ":/opt/homebrew/bin/"))
;;       (setq-default exec-path (append exec-path '("/opt/homebrew/bin/")))))

;; ------------------------------------------------------------------------
;; js-mode configs
;; ------------------------------------------------------------------------
(setq-default js-indent-level 4)

;; ------------------------------------------------------------------------
;; protobuf configs
;; ------------------------------------------------------------------------
; 4-space indent
(defconst my-protobuf-style
  '((c-basic-offset . 4)
    (indent-tabs-mode . nil)))

(add-hook 'protobuf-mode-hook
          (lambda () (c-add-style "my-style" my-protobuf-style t)))
