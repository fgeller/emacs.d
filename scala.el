(setq compilation-sbt-regexp '(sbt "^\\[error\\] \\(/[^:]+\\):\\([0-9]+\\):" 1 2))
(add-to-list 'compilation-error-regexp-alist 'sbt)
(add-to-list 'compilation-error-regexp-alist-alist compilation-sbt-regexp)

(defun compilation-find-file-recursively ()
  (find-file-recursively (match-string 1) default-directory))

(setq compilation-sbt-stacktrace-regexp
      '(sbt-stacktrace "[[:space:]]+at [^(]+(\\([^:]+\\):\\([[:digit:]]+\\))$" compilation-find-file-recursively 2))
(add-to-list 'compilation-error-regexp-alist 'sbt-stacktrace)
(add-to-list 'compilation-error-regexp-alist-alist compilation-sbt-stacktrace-regexp)

(use-package request :commands request :ensure request)
(defun scalariform-daemon-format-file ()
  (when (and (boundp 'scalariform-preferences-file)
             scalariform-preferences-file)
    (request
     "http://127.0.0.1:5474/format"
     :params `((fileName . ,buffer-file-name)
               (preferencesFile . ,scalariform-preferences-file))
     :sync t)
    (revert-buffer t t)))

(defun start-scalariform-daemon ()
  (interactive)
  (let ((buf-name "*scalariform-daemon*")
        (default-directory (expand-file-name "~/code/scalariform-daemon/")))
    (if (get-buffer buf-name)
        (with-current-buffer buf-name (recompile))
      (compile "sbt run")
      (with-current-buffer "*compilation*" (rename-buffer buf-name)))))

(defun scala-next-test-forward ()
  (interactive)
  (scala-next-test nil))

(defun scala-next-test-backward ()
  (interactive)
  (scala-next-test t))

(defun scala-next-test (backwards)
  (let ((test-prefix "\\(it(\\|describe(\\)"))
    (when (looking-at-p test-prefix) (forward-word))
    (when (if backwards
              (re-search-backward test-prefix (point-min))
            (re-search-forward test-prefix (point-max)))
      (unless backwards (backward-word)))))

(defun start-sbt ()
  (interactive)
  (let ((buf-name (format "*sbt [%s]*" default-directory)))
    (if (get-buffer buf-name)
        (switch-to-buffer buf-name)
      (compile "sbt" t)
      (with-current-buffer "*compilation*"
        (font-lock-mode 1)
        (rename-buffer buf-name)))))

(defun scala-customizations ()
  (setq tab-width 2)
  (font-lock-mode -1)
  (subword-mode 1)
  (yas-minor-mode 1)
  (add-hook 'after-save-hook 'scalariform-daemon-format-file nil 'make-it-local))

(use-package scala-mode2 :ensure scala-mode2
  :config
  (add-hook 'scala-mode-hook 'scala-customizations))

(use-package scala-errors
  :config (setq scala-errors-default-display-errors-function (lambda (b))))

(defun scala-ignore-all-tests ()
  (interactive)
  (save-excursion
    (replace-regexp "\\bit(\\(s\\)?\"" "ignore(\\1\"" nil (point-min) (point-max)))
  (unless current-prefix-arg
    (save-excursion
      (search-backward "ignore(" nil)
      (replace-match "it(" nil t))))

(defun scala-enable-all-tests ()
  (interactive)
  (save-excursion
    (replace-regexp "\\bignore(\\(s\\)?\"" "it(\\1\"" nil (point-min) (point-max))))
