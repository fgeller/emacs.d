(setq org-startup-folded 'showall)

(setq org-log-done 'time)

(setq
 org-special-ctrl-k t
 org-special-ctrl-a t)

(setq calendar-week-start-day 1)

(setq
 org-hide-emphasis-markers t
 org-hide-leading-stars t)

(setq org-edit-timestamp-down-means-later t)

(setq org-footnote-auto-label 'plain)

(defun org-mode-per-buffer-customization ()
  (yas-minor-mode)
  (turn-on-auto-fill))

(add-hook 'org-mode-hook 'org-mode-per-buffer-customization)

(setq org-enforce-todo-checkbox-dependencies t)

(setq org-loop-over-headlines-in-active-region t)

(setq org-agenda-sticky t)

(add-hook 'org-mode-hook '(lambda () (interactive) (font-lock-mode 1)))

(setq
 org-directory (expand-file-name "~/orgs")
 org-default-tasks-file (concat org-directory "/Tasks.org")
 org-default-notes-file (concat org-directory "/Notes.org")
 org-agenda-files (list (concat org-directory "/Tasks.org")))

(eval-after-load 'org
  (dolist (org-mod '(org-crypt org-info org-eshell))
    (require org-mod)))

(defun org-schedule-and-refile ()
  (interactive)
  (call-interactively 'org-schedule)
  (org-refile))

(setq org-todo-keywords '(
                    (sequence "TODO(t)" "STARTED(s)" "|" "DONE(d!/!)")
                    (sequence "WAITING(w@/!)" "|" "SUPERSEDED(u!/!)" "CANCELLED(c@/!)")))

(setq org-agenda-include-diary t)

(setq org-agenda-span 'day)

(setq org-agenda-remove-tags nil)

(setq org-agenda-tags-column -125)

(setq org-agenda-start-on-weekday nil)

(setq org-agenda-window-setup 'current-window)

(setq org-agenda-custom-commands
      `(;; match those tagged with :inbox:, are not scheduled, are not DONE.
        ("p" "[p]ersonal inbox" tags "+inbox+personal")
        ("w" "[w]ork inbox" tags "+inbox+work")
        ("n" "Find a TAGged note" tags "" ((org-agenda-archives-mode t)))))

(setq org-agenda-sorting-strategy '((agenda habit-down time-up todo-state-down)))

(defadvice org-agenda (around org-agenda-fullscreen activate)
  (window-configuration-to-register :org-agenda-fullscreen)
  ad-do-it
  (delete-other-windows))

(defadvice org-agenda-quit (around org-agenda-quit-fullscreen activate)
  ad-do-it
  (jump-to-register :org-agenda-fullscreen))

(setq org-clock-persist t)
(org-clock-persistence-insinuate)

(setq org-clock-in-resume t)

(setq org-clock-in-switch-to-state "STARTED")

(setq org-clock-into-drawer t)

(setq org-clock-out-remove-zero-time-clocks t)

(defun schedule-task-now ()
  (interactive)
  (let ((now (with-temp-buffer (org-time-stamp '(16)) (buffer-string))))
    (org-schedule nil now)
    (message "Scheduled started task for now")))

(add-hook 'org-clock-in-hook 'schedule-task-now)

(setq org-agenda-clockreport-parameter-plist '(:link t :maxlevel 5))

(add-hook 'org-capture-before-finalize-hook
          'org-refile-captured-entry-to-scheduled)

(defun org-refile-captured-entry-to-scheduled ()
  (save-excursion
    (org-capture-goto-last-stored)
    (when (or (org-get-scheduled-time (point))
              (org-get-deadline-time (point)))
      (let* ((scheduled-path (list org-default-tasks-file "Scheduled"))
             (target-marker (org-find-olp scheduled-path)))
        (org-refile nil nil (list
                             "Scheduled"
                             org-default-tasks-file
                             nil ;; re not needed
                             (marker-position target-marker)))))))

(setq org-capture-templates
      `(("t" "Task"
         entry (file+olp ,org-default-tasks-file "Inbox" "Personal")
         "* TODO %?\n\n")
        ("w" "Work task"
         entry (file+olp ,org-default-tasks-file "Inbox" "Work")
         "* TODO %? :work:\n\n")
        ("n" "Note"
         entry (file+headline ,org-default-notes-file "Notes")
         "* %?\n\n  %i\n")
        ("b" "Bookmark"
         entry (file+headline ,(expand-file-name "Bookmarks.org" org-directory) "Bookmarks")
         "* %?\n\n  %c%i\n")
        ("s" "Scratch"
         entry (file+headline ,(expand-file-name "scratch.org" user-emacs-directory) "Scratch")
         "* Scratch it %U\n%i\n   #+begin_src text\n%?\n   #+end_src\n")))

(setq
 org-outline-path-complete-in-steps nil
 org-refile-use-outline-path 'file
 org-refile-targets  '((nil :maxlevel . 5) (org-agenda-files :maxlevel . 5)))

(setq org-src-window-setup 'current-window)

(setq org-src-fontify-natively nil)

(setq org-babel-load-languages '((emacs-lisp . t) (scala . t) (clojure . t)))
