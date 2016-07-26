(use-package go-mode
  :ensure go-mode
  :commands go-mode)

(defun golang-customizations ()
  (subword-mode 1)
  (set (make-local-variable 'company-backends) '(company-go))
  (yas-minor-mode 1)
  (setq gofmt-command "goimports")
  (font-lock-mode 1)
  (setq scala-errors--error-re
        (rx bol
            (* space)
            (group (+ (not (any ":\n")))) ":"
            (group (+ (not (any ":\n")))) ":" (* space)
            (group (+ nonl))
            eol))
  (setq scala-errors--error-column-re nil)
  (define-key go-mode-map (kbd "M-.") 'godef-jump)
  (add-hook 'before-save-hook #'gofmt-before-save))

(add-hook 'go-mode-hook 'golang-customizations)

(defun go-ignore-all-tests ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (replace-regexp
     "func Test\\([^(]+\\)("
     "func IgnoreTest\\1("
     nil
     (point-min)
     (point-max))))

(defun go-enable-all-tests ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (replace-regexp
     "func IgnoreTest\\([^(]+\\)("
     "func Test\\1("
     nil
     (point-min)
     (point-max))))
