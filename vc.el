(add-hook 'vc-annotate-mode-hook
	  (lambda () (setq show-trailing-whitespace nil)))

(defun vc-git-annotate-command (file buf &optional rev)
  (let ((name (file-relative-name file)))
    (vc-git-command buf 'async nil "blame" "--date=short" "-C" "-C" rev "--" name)))

(use-package magit
  :defer t
  :ensure magit
  :init (setq git-commit-summary-max-length 72))
