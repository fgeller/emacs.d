(use-package pulse)
(use-package recentf)
(use-package seq)

(use-package ivy :ensure swiper
  :defer t
  :init
  (setq ivy-use-virtual-buffers t)
  :config
  (ivy-mode 1))

(use-package counsel :ensure counsel
  :commands (counsel-more-chars))

(defun strip-text-properties (txt)
  "Removes text properties from TXT"
  (set-text-properties 0 (length txt) nil txt)
  txt)

(defun add-ivy-action (str action)
  "Adds ACTION as text-property to start of STR"
  (put-text-property 0 1 'ivy-action action str))

(defun get-ivy-action (str)
  "Returns text-property `ivy-action' at start of STR"
  (get-text-property 0 'ivy-action str))

(defun string-copy (str)
  "Uses `substring' to return a copy of STR without text properties"
  (substring str 0 (length str)))

(defun ivy-add-action-to-candidates (candidates action)
  "Copies each entry in CANDIDATES and adds ACTION as a text property to it"
  (mapcar (lambda (bn)
            (let ((c (string-copy bn)))
              (add-ivy-action c action)
              c))
          candidates))

(defun ivy-actioner (c)
  "Helper to execute closure saved as ivy action."
  (funcall (get-ivy-action c) c))

(defun enable-ivy-calling ()
  (setq ivy-calling t))

(defmacro with-ivy-calling (&rest body)
  `(unwind-protect
       (progn
         (advice-add 'ivy--minibuffer-setup :before 'enable-ivy-calling)
         ,@body)
     (advice-remove 'ivy--minibuffer-setup 'enable-ivy-calling)))

(defun ivy-apropos-candidates ()
  (let ((start-time (current-time))
        cs)
    (mapatoms
     (lambda (e)
       (cond
        ((fboundp e)
         (let ((c (string-copy (symbol-name e))))
           (add-ivy-action c 'describe-function)
           (push c cs)))
        ((or (get e 'variable-documentation)
             (and (boundp e) (not (keywordp e))))
         (let ((c (string-copy (symbol-name e))))
           (add-ivy-action c 'describe-variable)
           (push c cs))))))
    cs))

(defun ivy-apropos ()
  (interactive)
  (ivy-read "Describe: " (ivy-apropos-candidates)
   :initial-input (thing-at-point 'symbol)
   :action (lambda (s) (funcall (get-ivy-action s) (intern s)))))

(defvar ivy-ag-extra-argument "" "Gets appended to ag invocation")

;; TODO create temp file when no file associated?
(defun ivy-ag-function (string)
  "Grep in the current directory for STRING."
  (if (< (length string) 3)
      (counsel-more-chars 3)
    (let* ((default-directory counsel--git-grep-dir)
           (re (counsel-unquote-regex-parens
                (setq ivy--old-re (ivy--regex string))))
           (fn (with-ivy-window (file-relative-name (buffer-file-name) counsel--git-grep-dir)))
           (qfn (shell-quote-argument fn))
           (qre (shell-quote-argument re))
           (ea ivy-ag-extra-argument)
           (cmd (format (concat "(ag %s --vimgrep %s -- %s || exit 0)"
                                " && (ag %s --ignore %s --vimgrep %s || exit 0)")
                        ea qre qfn
                        ea qfn qre)))
      (counsel--async-command cmd)
      nil)))

(defun ivy-ag-action (c)
  (counsel-git-grep-action c)
  (with-ivy-window
    (let ((recenter-positions '(middle))
          (pulse-delay (* 3 pulse-delay)))
      (recenter-top-bottom)
      (pulse-momentary-highlight-one-line (point) 'swiper-line-face))))

(defun ivy-ag-with-thing-at-point ()
  "ag with thing at point, preselecting match where point is and defaulting to current git root."
  (interactive)
  (with-ivy-calling
   (let ((init-dir (or (locate-dominating-file default-directory ".git")
                       default-directory))
         (tap (strip-text-properties (or (thing-at-point 'symbol) ""))))
     (setq counsel--git-grep-dir init-dir)
     (ivy-read "ag: " 'ivy-ag-function
               :initial-input tap
               :dynamic-collection t
               :history 'counsel-git-grep-history
               :action 'ivy-ag-action
               :preselect
               (with-ivy-window
                 (when (buffer-file-name)
                   (let* ((fn (file-name-nondirectory (buffer-file-name)))
                          (ln (line-number-at-pos)))
                     (format "%s:%s:" fn ln))))
               :unwind
               (lambda ()
                 (counsel-delete-process)
                 (swiper--cleanup))))))


(defun ivy-ag ()
  "ag with thing at point, preselecting match where point is and defaulting to current git root."
  (interactive)
  (with-ivy-calling
   (let ((init-dir (or (locate-dominating-file default-directory ".git")
                       default-directory)))
     (setq counsel--git-grep-dir init-dir)
     (ivy-read "ag: " 'ivy-ag-function
               :dynamic-collection t
               :history 'counsel-git-grep-history
               :action 'ivy-ag-action
               :unwind
               (lambda ()
                 (counsel-delete-process)
                 (swiper--cleanup))))))

(defun ivy-ag-with-thing-at-point-in-main ()
  "ag with thing at point, preselecting match where point is and defaulting to current git root."
  (interactive)
  (with-ivy-calling
   (let ((ivy-ag-extra-argument (concat "--ignore test")))
     (ivy-ag-with-thing-at-point))))

(defun git-ls-files ()
  (let ((grt (locate-dominating-file default-directory ".git")))
    (when grt
      (let* ((default-directory grt))
        (split-string (shell-command-to-string "git ls-files --full-name --") "\n" t)))))

(defun ivy-git-files-candidates ()
  (let ((bfns (mapcar 'buffer-file-name (buffer-list))))
    (ivy-add-action-to-candidates
     (cl-remove-if (lambda (gf) (member gf bfns)) (git-ls-files))
     (lambda (n) (with-ivy-window
                   (let ((grt (locate-dominating-file default-directory ".git"))
                         (inhibit-message t))
                     (find-file (expand-file-name n grt))))))))

(defun ivy-buffer-name-candidates ()
  (ivy-add-action-to-candidates
   (remove-if (lambda (c) (or (string-prefix-p " *" c)
			      (string-prefix-p "*Minibuf-" c)))
              (mapcar 'buffer-name (buffer-list)))
   (lambda (n) (with-ivy-window (switch-to-buffer n nil 'force-same-window)))))

(defun ivy-org-heading-action (f)
  (lexical-let ((fn f))
    (lambda (c)
      (with-ivy-window
        (find-file fn)
        (goto-char (point-min))
        (re-search-forward (concat "^" (regexp-quote c)))
        (org-beginning-of-line)
        (recenter-top-bottom)))))

(defun ivy-org-heading-candidates ()
  (apply (lambda (ls) (seq-concatenate 'list ls))
   (mapcar
    (lambda (b)
      (let ((cs (with-temp-buffer
                  (insert (with-current-buffer (find-file-noselect b) (buffer-string)))
                  (keep-lines org-heading-regexp (point-min) (point-max))
                  (mapcar 'strip-text-properties
                          (split-string (buffer-string) "\n" t "[      ]*")))))
        (ivy-add-action-to-candidates cs (ivy-org-heading-action b))))
    org-agenda-files)))

(defun ivy-recentf-candidates ()
  (ivy-add-action-to-candidates recentf-list '(find-file)))

(defun ivy-jump-candidates ()
  "Returns a list of candidates for jumping to with associated actions as text properties"
  (let* ((start-time (current-time))
         (bufs (ivy-buffer-name-candidates))
         (gfs (ivy-git-files-candidates))
         (rfs (ivy-recentf-candidates))
         (cs (seq-uniq (seq-concatenate 'list bufs gfs rfs)))
         (elapsed (time-subtract (current-time) start-time)))
    cs))

(defun ivy-jump ()
  "ivy completion for common jump targets (buffers, git-ls-files, recentf)"
  (interactive)
  (with-ivy-calling
   (ivy-read "jump "
             (ivy-jump-candidates)
             :preselect (buffer-name (other-buffer (current-buffer)))
             :action 'ivy-actioner)))

(defun project-directories ()
  (seq-concatenate
   'list
   (mapcar
    (lambda (gd) (file-relative-name (file-name-directory gd) (expand-file-name "~")))
    (split-string (shell-command-to-string "find ~/src -maxdepth 4 -name .git -type d") "\n" t))
   `(".emacs.d")))

(defun ivy-jump-to-project-action (p)
  (with-ivy-window
    (magit-status (expand-file-name p "~"))))

(defun ivy-jump-to-project-candidates ()
  (ivy-add-action-to-candidates (project-directories) 'ivy-jump-to-project-action))

(defun ivy-jump-to-project ()
  (interactive)
  (ivy-read
   "project "
   (ivy-jump-to-project-candidates)
   :action 'ivy-actioner))

(defun ivy-git-ls-files-project-action (p)
  (with-ivy-window
    (let ((default-directory (expand-file-name p "~")))
      (ivy-read
       "file "
       (ivy-git-files-candidates)
       :action 'ivy-actioner))))

(defun ivy-git-ls-files-project-candidates ()
  (ivy-add-action-to-candidates (project-directories) 'ivy-git-ls-files-project-action))

(defun ivy-git-ls-files-project ()
  (interactive)
  (ivy-read
   "project "
   (ivy-git-ls-files-project-candidates)
   :action 'ivy-actioner))

(defun string-trim (str)
  (replace-regexp-in-string
   "\\`[ \t]*"
   ""
   (replace-regexp-in-string "[ \t]*\\'" "" str)))

(defun ivy-mark-ring-action (buf pos)
  (lexical-let* ((buf buf)
                 (pos pos))
    (lambda (mn)
      (with-ivy-window
        (switch-to-buffer buf nil 'force-same-window)
        (goto-char pos)
        (let ((recenter-positions '(middle))
              (pulse-delay (* 3 pulse-delay)))
          (recenter-top-bottom)
          (pulse-momentary-highlight-one-line (point) 'swiper-line-face))))))

(defun ivy-pos-description (buf pos)
  (with-current-buffer buf
    (save-excursion
      (goto-char pos)
      (format "%5s: %s|%s"
              (line-number-at-pos)
              (string-trim (buffer-substring (line-beginning-position) (point)))
              (string-trim (buffer-substring (point) (line-end-position)))))))

(defun ivy-mark-ring-candidates (ring)
  (let* ((start-time (current-time))
         cs)
    (mapc
     (lambda (m)
       (when (and (marker-buffer m) (marker-position m))
         (let* ((buf (marker-buffer m))
                (bn (buffer-name buf))
                (pos (marker-position m))
                (des (ivy-pos-description buf pos))
                (can (format "%15.15s:%s" bn des)))
           (add-ivy-action can (ivy-mark-ring-action buf pos))
           (push can cs))))
     ring)
    cs))

(defun ivy-global-mark-ring ()
  (interactive)
  (with-ivy-calling
   (ivy-read
    "global mark "
    (ivy-mark-ring-candidates global-mark-ring)
    :action 'ivy-actioner)))

(defun ivy-local-mark-ring ()
  (interactive)
  (with-ivy-calling
   (ivy-read
    "local mark "
    (ivy-mark-ring-candidates mark-ring)
    :action 'ivy-actioner)))

(defun swiper-with-thing-at-point ()
  (interactive)
  (swiper (thing-at-point 'symbol))
  (let ((recenter-positions '(middle))
        (pulse-delay (* 3 pulse-delay)))
    (recenter-top-bottom)
    (pulse-momentary-highlight-one-line (point) 'swiper-line-face)))

(defun swiper-tweaked ()
  (interactive)
  (swiper)
  (let ((recenter-positions '(middle))
        (pulse-delay (* 3 pulse-delay)))
    (recenter-top-bottom)
    (pulse-momentary-highlight-one-line (point) 'swiper-line-face)))
