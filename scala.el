(defun scala-customizations ()
  (setq tab-width 2)
  (font-lock-mode -1)
  (subword-mode 1)
  (yas-minor-mode 1))

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
