(add-hook 'vc-annotate-mode-hook (lambda () (setq show-trailing-whitespace nil)))

(defun vc-git-annotate-command (file buf &optional rev)
  (let ((name (file-relative-name file)))
    (vc-git-command buf 'async nil "blame" "--date=short" "-C" "-C" rev "--" name)))

(use-package magit
  :ensure magit
  :config (setq git-commit-summary-max-length 72))

;; (setq magit-display-buffer-function
;;       (lambda (b)
;;         (display-buffer b '(display-buffer-same-window))))
