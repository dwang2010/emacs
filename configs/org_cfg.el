;; ------------------------------------------------------------------------
;; org-mode - note taking on steroids
;; ------------------------------------------------------------------------
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

(use-package org
  :hook (org-mode . turn-on-auto-fill)
  :config
  ;; startup behavior
  (setq-default org-startup-indented t)
  (setq-default org-indent-indentation-per-level 1)
  (setq-default org-pretty-entities nil) ;; don't show special utf-8 symbols
  (setq-default org-src-preserve-indentation nil)
  (setq-default org-edit-src-content-indentation 0) ; don't indent src blk contents

  ;; customize heading sizes
  (set-face-attribute 'org-level-1 nil :height 1.15)
  (set-face-attribute 'org-level-2 nil :height 1.10)
  (set-face-attribute 'org-level-3 nil :height 1.05)
  (set-face-attribute 'org-level-4 nil :height 1.0)

  ;; customize some faces (linked to theme)
  (set-face-attribute 'org-table nil :foreground "#a991f1")
  (set-face-attribute 'org-code nil :foreground "#e69055")

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
     (lisp . t)
     (go . t)))
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
  (setq-default org-tags-column -80)
  (setq-default org-auto-align-tags t)

  (setq-default org-log-done 'time) ; auto add completion time on TODO close
  (setq-default org-bookmark-names-plist nil) ; no refile / capture bookmarks
  (setq-default org-blank-before-new-entry '((heading . auto) (plain-list-item . auto)))

  ;; org-agenda - dashboard view of tasks
  ;; :       - add / remove tags
  ;; t       - change todo state
  ;; C-c C-W - refile
  (global-set-key (kbd "<f1>") (lambda () (interactive) (org-agenda nil "n")))
  (set-face-attribute 'org-agenda-structure nil :foreground "#00bfff" :height 1.25)
  (set-face-attribute 'org-agenda-date-weekend nil :foreground "#4d4d4d")
  (set-face-attribute 'org-agenda-date-today nil :foreground "white")
  (setq-default org-agenda-files '("~/notes/ttd.org"))
  (setq-default org-agenda-window-setup 'current-window)
  (setq org-agenda-custom-commands
        '(("n" "Main Agenda"
           (;; 20 day agenda view
            (agenda "" ((org-agenda-block-separator nil)
                        (org-agenda-overriding-header "--- Upcoming ---\n")
                        (org-agenda-format-date "%F %a")
                        (org-agenda-span 20)
                        (org-agenda-start-day "0d")
                        (org-agenda-start-on-weekday 0) ; only works for 7 / 14 day span
                        (org-deadline-warning-days 0)
                        (org-agenda-skip-function
                         '(org-agenda-skip-entry-if 'nottodo '("TODO")))))

            ;; active tasks without date filtering
            (tags-todo "-backlog" ((org-agenda-block-separator nil)
                                   (org-agenda-overriding-header "\n--- Active ---\n")))

            ;; backlog tasks
            (tags-todo "backlog" ((org-agenda-block-separator nil)
                                  (org-agenda-overriding-header "\n--- Backlog ---\n")))))))

  ;; org-capture - "capture" notes at any time, quickly
  ;; use tags to add additional context for filtering
  ;; e.g. "project" or "attendees" or "meeting_type" etc.
  ;; C-c C-d - add deadline
  ;; C-c C-s - add schedule
  (global-set-key (kbd "<f2>") 'org-capture)
  (setq-default org-default-ttd-file "~/notes/ttd.org")
  (setq-default org-default-random-file "~/notes/random.org")
  (setq-default org-default-meetings-file "~/notes/meetings.org")
  (setq-default org-capture-templates
                '(;; code related tasks
                  ("t" "Task" entry
                   (file+headline org-default-ttd-file "Tasks")
                   "* TODO [#2] %?\n" :prepend t)

                  ("c" "Task w/ Code Ref" entry
                   (file+headline org-default-ttd-file "Tasks with Ref")
                   "* TODO [#2] %?\n\n%(get-org-blk-code-snippet \"%F\")"
                   :prepend t)

                  ("b" "Backlog" entry
                   (file+headline org-default-ttd-file "Tasks")
                   "* TODO [#3] %? %(org-set-tags \"backlog\")" :prepend t)

                  ;; meeting related notes
                  ("g" "Group Meeting" plain
                   (file+headline org-default-meetings-file "Group")
                   (file "~/.emacs.d/configs/org_templates/meeting.org")
                   :prepend t)

                  ("1" "1:1 Meeting" entry
                   (file+headline org-default-meetings-file "1:1")
                   "* %T w/ %?" :empty-lines-after 1 :prepend t)

                  ("p" "Personal Call" entry
                   (file+headline org-default-meetings-file "Personal Call")
                   "* %T w/ %?" :empty-lines-after 1 :prepend t)

                  ;; completely arbitrary, diary style notes
                  ("r" "Random" entry
                   (file+headline org-default-random-file "Random Notes")
                   "* %T\n%?" :empty-lines-after 1 :prepend t)))

  ;; configure capture popup behavior
  (add-to-list 'display-buffer-alist
               ;; ( BUFFER-MATCHER
               ;;   LIST-OF-DISPLAY-FUNCTIONS
               ;;   &optional PARAMETERS )
               '("\\(\\*Capture\\*\\|CAPTURE-.*\\|\\*Org Select\\*\\)"
                 (display-buffer-same-window display-buffer-below-selected)
                 (window-height . 0.4)))

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
  (setq-default org-export-with-toc nil) ; don't export table of contents
  (setq-default org-ascii-headline-spacing nil) ; remove whitespace around headlines
  (setq-default org-ascii-text-width 10000) ; hack to unfill paragraphs
  (setq-default org-ascii-inner-margin 0)) ; remove body indentation when exporting

;; modify call to org-capture to not hide other buffers
(defun my-org-capture-place-template-dont-delete-windows (oldfun &rest args)
  (cl-letf (((symbol-function 'delete-other-windows) 'ignore))
    (apply oldfun args)))

(with-eval-after-load "org-capture"
  (advice-add 'org-capture-place-template
              :around 'my-org-capture-place-template-dont-delete-windows))

;; make org bullets fancy
(use-package org-superstar
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-superstar-mode 1))))

;; more evident task priority
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
