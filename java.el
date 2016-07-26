(defun java-customizations ()
  (setq tab-width 2)
  (add-hook 'before-save-hook 'maybe-cleanup-whitespace nil 'local))

(add-hook 'java-mode-hook 'java-customizations)
