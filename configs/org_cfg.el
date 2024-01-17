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
  :config
  ;; startup behavior
  (setq-default org-startup-indented t)
  (setq-default org-indent-indentation-per-level 1)
  (setq-default org-pretty-entities nil) ;; don't show special utf-8 symbols

  ;; customize heading sizes
  (set-face-attribute 'org-level-1 nil :height 1.15)
  (set-face-attribute 'org-level-2 nil :height 1.10)
  (set-face-attribute 'org-level-3 nil :height 1.05)
  (set-face-attribute 'org-level-4 nil :height 1.0)

  ;; customize emphasis markers
  (setq-default org-hide-emphasis-markers t)
  (setq-default org-emphasis-alist
        '(("*" (bold :foreground "orange"))
          ("/" italic)
          ("_" underline)
          ("=" (:background "maroon" :foreground "white"))
          ("~" (:background "deep sky blue" :foreground "MidnightBlue"))
          ("+" (:strike-through t))))

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
  (setq-default org-tags-column -80)
  (setq-default org-auto-align-tags t)

  ;; automatically add completion time when closing TODO
  (setq-default org-log-done 'time)

  ;; org-agenda - dashboard view of tasks
  ;; :       - add tags
  ;; C-k     - remove todo item
  ;; C-c C-W - refile
  (global-set-key (kbd "<f1>") (lambda () (interactive) (org-agenda nil "n")))
  (set-face-attribute 'org-agenda-structure nil :foreground "#00bfff" :height 1.25)
  (set-face-attribute 'org-agenda-date-weekend nil :foreground "#4d4d4d")
  (set-face-attribute 'org-agenda-date-today nil :foreground "white")
  (setq-default org-agenda-files '("~/notes/ttd.org"))
  (setq-default org-agenda-window-setup 'current-window)
  (setq org-agenda-custom-commands
        '(("n" "Main Agenda"
           (;; 2-week agenda view
            (agenda "" ((org-agenda-block-separator nil)
                        (org-agenda-overriding-header "--- Upcoming Deadlines ---\n")
                        (org-agenda-format-date "%F %a")
                        (org-agenda-span 25)
                        (org-agenda-start-day "-5d")
                        (org-agenda-start-on-weekday 0) ; only works for 7 / 14 day span
                        (org-deadline-warning-days 0)
                        (org-agenda-skip-function
                         '(org-agenda-skip-entry-if 'nottodo '("TODO")))))

            ;; blob of all todo, without date filtering
            (alltodo "" ((org-agenda-block-separator nil)
                         (org-agenda-overriding-header "\n--- Global TODO ---\n")))))))

  ;; org-capture - "capture" notes at any time, quickly
  ;; use tags to add additional context for filtering
  ;; e.g. "project" or "attendees" or "meeting_type" etc.
  ;; C-c C-d - add deadline
  ;; C-c C-s - add schedule
  (global-set-key (kbd "<f2>") 'org-capture)
  (setq-default org-default-notes-file "~/notes/ttd.org")
  (setq-default org-default-meetings-file "~/notes/meetings.org")
  (setq-default org-capture-templates
                '(;; task, no reference
                  ("t" "Task" entry
                   (file+headline org-default-notes-file "Tasks")
                   "* TODO [#2] %?\nDEADLINE: " :prepend t)

                  ;; task with code reference
                  ("c" "Task w/ Code Ref" entry
                   (file+headline org-default-notes-file "Tasks with Ref")
                   "* TODO [#2] %?\nDEADLINE: \n\n%(get-org-blk-code-snippet \"%F\")"
                   :prepend t)

                  ;; task backlog
                  ("b" "Backlog" entry
                   (file+headline org-default-notes-file "Backlog")
                   "* TODO [#3] %?" :prepend t)

                  ;; meeting related notes
                  ("m" "Meeting Notes" entry
                   (file+headline org-default-meetings-file "Meeting Notes")
                   "* %t %^{Name}\n%?" :empty-lines-after 1 :prepend t)))

  ;; org-refile - moving stuff between org files
  (setq-default org-reverse-note-order t) ; prepend on refile
  (setq-default org-blank-before-new-entry nil) ; prepend doesn't need another blank
  (setq-default org-refile-use-outline-path 'file) ; allows for granular refile targetting
  (setq-default org-outline-path-complete-in-steps nil) ; allows for fuzzy find refiling
  (advice-add 'org-refile :after 'org-save-all-org-buffers) ; save all after refiling
  (setq-default org-refile-targets
                '((nil :maxlevel . 3) ; nil equates to "current buffer"
                  (org-agenda-files :maxlevel . 1)
                  ("~/notes/archive.org" :maxlevel . 2)))) ; * year / ** month

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