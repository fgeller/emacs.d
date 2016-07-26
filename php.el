(use-package php-mode
  :ensure php-mode
  :commands php-mode)

(defun php-customizations ()
  (setq tab-width 4)
  (add-hook 'before-save-hook 'maybe-cleanup-whitespace nil 'local))

(add-hook 'php-mode-hook 'php-customizations)
