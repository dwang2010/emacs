;; ------------------------------------------------------------------------
;; org-mode - note taking on steroids
;; ------------------------------------------------------------------------
;; ensure org version >= 9.7.4
(my-ensure-package-version 'org "9.7.4")

(defun my-org-binding-configs ()
  (local-unset-key (kbd "C-,")) ;; cycle org agenda files
  (local-set-key (kbd "C-c b") 'my-quick-add-emphasis)
  (local-set-key (kbd "C-c ,") 'my-quick-add-src-blk)
  (local-set-key (kbd "C-c C-,") 'my-quick-add-src-blk))

(use-package org
  :bind ("C-c C-o" . org-open-at-point-global)
  :hook ((org-mode . turn-on-auto-fill)
         (org-mode . my-org-binding-configs))
  :config
  ;; startup behavior
  (setq-default org-startup-indented t)
  (setq-default org-indent-indentation-per-level 1)
  (setq-default org-pretty-entities nil) ;; don't show special utf-8 symbols
  (setq-default org-src-preserve-indentation nil)
  (setq-default org-edit-src-content-indentation 0) ; don't indent src blk contents
  (setq-default org-fold-core-style 'overlays) ; fix issues with subtree folding

  ;; customize heading sizes
  (set-face-attribute 'org-level-1 nil :height 1.10)
  (set-face-attribute 'org-level-2 nil :height 1.05)
  (set-face-attribute 'org-level-3 nil :height 1.00)

  ;; customize some faces
  (set-face-attribute 'org-table nil :foreground "#a991f1")
  (set-face-attribute 'org-code nil :foreground "#e69055")
  (set-face-attribute 'org-agenda-structure nil :foreground "#00bfff" :height 1.2)
  (set-face-attribute 'org-link nil :foreground "#79bac4" :bold nil)
  (if (eql dcw-dark-theme-flag t)
      (progn
        (set-face-attribute 'org-todo nil :foreground "#cd5555")
        (set-face-attribute 'org-checkbox nil :foreground "#f5f5f5")
        (set-face-attribute 'org-agenda-date-weekend nil :foreground "#4d4d4d")
        (set-face-attribute 'org-agenda-date-today nil :foreground "white")))

  ;; customize emphasis markers
  (setq-default org-hide-emphasis-markers t)
  (setq-default org-emphasis-alist
        '(("*" (bold :foreground "orange"))
          ("/" italic)
          ("_" underline)
          ("=" org-verbatim)
          ("~" org-code)
          ("+" (:strike-through t))))

  ;; scale inline images if attr_org present, otherwise use image width
  ;; use C-c C-x C-v to render inline images
  ;; alternatively add <#+STARTUP: inlineimages> to individual files
  (setq-default org-image-actual-width nil)

  ;; babel configuration to load languages
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)
     (lisp . t)))
  ;; set org-babel for desired python executable
  (setq-default org-babel-python-command "python3")
  ;; potentially risky: remove confirmation when executing code blocks
  (setq-default org-confirm-babel-evaluate nil)

  ;; C-c C-o opens link in same buffer rather than new buffer
  (setq-default org-link-frame-setup
        '((vm . vm-visit-folder-other-frame)
          (vm-imap . vm-visit-imap-folder-other-frame)
          (gnus . org-gnus-no-new-news)
          (file . find-file)
          (wl . wl-other-frame)))

  ;; tags related
  (setq-default org-tags-column -75)
  (setq-default org-auto-align-tags t)

  (setq-default org-log-done 'time) ; auto add completion time on TODO close
  (setq-default org-bookmark-names-plist nil) ; no refile / capture bookmarks
  (setq-default org-blank-before-new-entry '((heading . t) (plain-list-item . auto)))

  ;; org-agenda - dashboard view of tasks
  ;; :       - add / remove tags
  ;; t       - change todo state
  ;; C-c C-W - refile
  (global-set-key (kbd "<f1>") (lambda () (interactive) (org-agenda nil "n")))
  (setq-default org-agenda-files '("~/notes/ttd.org"))
  (setq-default org-agenda-window-setup 'current-window)
  (setq-default org-agenda-sorting-strategy '(priority-down alpha-up))
  (setq-default org-agenda-inhibit-startup t) ; attempt to speedup agenda
  (setq-default org-agenda-ignore-properties '(stats)) ; attempt to speedup agenda
  (setq org-agenda-custom-commands
        '(("n" "Main Agenda"
           (;; agenda view
            (agenda "" ((org-agenda-block-separator nil)
                        (org-agenda-overriding-header "UPCOMING\n")
                        (org-agenda-format-date "%F %a")
                        (org-agenda-span 14)
                        (org-agenda-start-day "0d")
                        (org-agenda-start-on-weekday 0) ; only on 7 / 14 day span
                        (org-deadline-warning-days 0)
                        (org-agenda-skip-function
                         '(org-agenda-skip-entry-if 'nottodo '("TODO")))))

            ;; active tasks without date filtering
            (tags-todo "-backlog-personal-emacs"
                       ((org-agenda-block-separator nil)
                        (org-agenda-overriding-header "\nACTIVE\n")))

            ;; backlog tasks
            (tags-todo "backlog"
                       ((org-agenda-block-separator nil)
                        (org-agenda-overriding-header "\nBACKLOG\n")))

            ;; personal tasks
            (tags-todo "personal"
                       ((org-agenda-block-separator nil)
                        (org-agenda-overriding-header "\nPERSONAL\n")))

            ;; emacs tasks
            (tags-todo "emacs"
                       ((org-agenda-block-separator nil)
                        (org-agenda-overriding-header "\nEMACS\n")))

            ;; closed this week
            (tags (concat "CLOSED>=\"" (my-get-target-week-sunday 0) "\"")
                  ((org-agenda-overriding-header "\nCLOSED: THIS WEEK\n")))

            ;; closed last week
            (tags (concat "CLOSED<=\"" (my-get-target-week-sunday 0) "\""
                          "+CLOSED>=\"" (my-get-target-week-sunday 1) "\"")
                  ((org-agenda-block-separator nil)
                   (org-agenda-overriding-header "\nCLOSED: LAST WEEK\n")))))))

  ;; org-capture - "capture" notes at any time, quickly
  ;; use tags to add additional context for filtering
  ;; e.g. "project" or "attendees" or "meeting_type" etc.
  ;; C-c C-d - add deadline
  ;; C-c C-s - add schedule
  (global-set-key (kbd "<f2>") 'org-capture)
  (setq-default org-default-ttd-file "~/notes/ttd.org")
  (setq-default org-default-meetings-file "~/notes/meetings.org")
  (setq-default org-default-random-file "~/notes/random.org")
  (setq-default org-capture-templates
                '(;; --- work related tasks ---
                  ("t" "Task" entry
                   (file+headline org-default-ttd-file "Inbox: Tag + Refile")
                   "* TODO [#2] %?\n" :prepend t)

                  ;; --- meeting related notes ---
                  ("m" "Meeting" plain
                   (file+headline org-default-meetings-file "Inbox: Tag + Refile")
                   (file "~/.emacs.d/configs/org_templates/meeting.org") :prepend t)

                  ("1" "1:1" entry
                   (file+headline org-default-meetings-file "Inbox: Tag + Refile")
                   "* %T 1:1 w/ %?" :prepend t)

                  ;; --- completely arbitrary, diary style notes ---
                  ("r" "Random" entry
                   (file+headline org-default-random-file "Random Notes")
                   "* %T\n%?" :empty-lines-after 1 :prepend t)))

  ;; configure capture popup behavior
  (add-to-list 'display-buffer-alist
               ;; ( BUFFER-MATCHER
               ;;   LIST-OF-DISPLAY-FUNCTIONS
               ;;   &optional PARAMETERS )
               '("\\(\\*Capture\\*\\|CAPTURE-\\|\\*Org Select\\*\\)"
                 (display-buffer-below-selected)))

  ;; org-refile - moving stuff between org files
  (setq-default org-reverse-note-order t) ; prepend on refile
  (setq-default org-refile-use-outline-path 'file) ; allows for granular refile targetting
  (setq-default org-outline-path-complete-in-steps nil) ; allows for fuzzy find refiling
  (advice-add 'org-refile :after 'org-save-all-org-buffers) ; save all after refiling
  (setq-default org-refile-targets
                '((nil :maxlevel . 3) ; nil equates to "current buffer"
                  (org-agenda-files :maxlevel . 1)
                  ("~/notes/archive.org" :maxlevel . 2))) ; * year / ** month

  ;; org export related
  ;; C-c C-e : open export dispatcher
  (setq-default org-export-backends '(ascii md))
  (setq-default org-export-with-toc nil) ; don't export table of contents
  (setq-default org-ascii-headline-spacing nil) ; remove whitespace around headlines
  (setq-default org-ascii-text-width 10000) ; hack to unfill paragraphs
  (setq-default org-ascii-inner-margin 0)) ; remove body indentation when exporting

;; remove mouse highlighting in agenda
(add-hook 'org-agenda-finalize-hook
          (lambda ()
            (make-local-variable 'mouse-highlight)
            (setq mouse-highlight nil)))

;; suppress 'org-element-at-point' warning for current org version
(setq-default warning-suppress-log-types '((org-element org-element-parser)))

;; ---------------------------------------------------------
;; make org bullets fancy
;; ---------------------------------------------------------
;; (use-package org-superstar
;;   :ensure t
;;   :config
;;   (add-hook 'org-mode-hook (lambda () (org-superstar-mode 1))))

;; ---------------------------------------------------------
;; more evident task priority
;; ---------------------------------------------------------
(use-package org-fancy-priorities
  :ensure t
  :hook (org-mode . org-fancy-priorities-mode)
  :config
  (setq-default org-priority-highest 0
                org-priority-default 1
                org-priority-lowest 3)
  (setq-default org-fancy-priorities-list '((?0 . "P0")
                                            (?1 . "P1")
                                            (?2 . "P2")
                                            (?3 . "P3")))
  (setq-default org-priority-faces '((?0 :foreground "#ff0000" :weight bold)
                                     (?1 :foreground "#ff7f00" :weight bold)
                                     (?2 :foreground "#ffc125" :weight bold)
                                     (?3 :foreground "#b3b3b3" :weight bold))))

;; ---------------------------------------------------------
;; presentation mode
;; ---------------------------------------------------------
(use-package visual-fill-column
  :ensure t
  :config
  (setq-default visual-fill-column-width 100)
  (setq-default visual-fill-column-center-text t))

(defun my-org-present-start-hook ()
  "Buffer configs to configure when starting presentation mode"
  (visual-fill-column-mode 1)
  (visual-line-mode 1)
  (display-line-numbers-mode -1)
  (setq header-line-format " ")
  (setq face-remapping-alist
        '(; body scaling
          (default (:height 1.4) fixed-pitch)
          ; padding from top
          (header-line (:height 3.0) fixed-pitch)
          ; top line title face
          (org-document-title (:height 3.0) org-document-title))))

(defun my-org-present-end-hook ()
  "Buffer configs to revert when exiting presentation mode"
  (visual-fill-column-mode 0)
  (visual-line-mode 0)
  (display-line-numbers-mode 1)
  (setq header-line-format nil)
  (setq face-remapping-alist '((default fixed-pitch default))))

(use-package org-present
  ;; init with 'org-present' command
  ;; left / right keys for slide movement
  ;; C-c C-q to exit
  :ensure t
  :hook ((org-present-mode . my-org-present-start-hook)
         (org-present-mode-quit . my-org-present-end-hook)))

;; ---------------------------------------------------------
;; helper functions
;; ---------------------------------------------------------
;; helper funcs to grab code snippet in referenced org SRC block
;; modified from: github.com/howardabrams/hamacs/blob/main/ha-capturing-notes.org
(defun get-fileref-blk-snippet (mode)
  (let* ((code-snippet (buffer-substring-no-properties (mark) (- (point) 1)))
         (file-name (buffer-file-name))
         (file-base (file-name-nondirectory file-name))
         (line-number (line-number-at-pos (region-beginning)))
         (initial-txt (format "[[file:%s::%s][%s::%s]]"
                              file-name line-number file-base line-number)))
    (format "%s\n#+begin_src %s\n%s\n#+end_src" initial-txt mode code-snippet)))

(defun get-org-blk-code-snippet (file)
  (with-current-buffer (find-buffer-visiting file)
    (let ((org-src-mode (replace-regexp-in-string "-mode" "" (format "%s" major-mode))))
      (get-fileref-blk-snippet org-src-mode))))

(defun my-get-next-sunday ()
  "Find decoded date of the next Sunday"
  (let ((d (decode-time)))
    (decoded-time-add d (make-decoded-time :day (% (- 7 (decoded-time-weekday d)) 7)))))

(defun my-get-target-week-sunday (weeks-before)
  "Find datestring (YYYY-MM-DD) of Sunday with integral 'weeks-before' current"
  (let ((weeks (+ 1 weeks-before)))
    (format-time-string "[%Y-%m-%d]"
                        (time-subtract
                         (encode-time (my-get-next-sunday))
                         (days-to-time (* 7 weeks))))))

(defun my-quick-add-emphasis ()
  "Enclose symbol with '=' for org emphasis"
  (require 'expand-region)
  (interactive) (er/mark-symbol) (org-emphasize ?\=))

(defun my-quick-add-src-blk ()
  "Auto add source block without prompting"
  (interactive) (org-insert-structure-template "src"))
