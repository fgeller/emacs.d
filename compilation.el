
(defun enable-font-lock () (font-lock-mode 1))
(add-hook 'compilation-mode-hook 'enable-font-lock)
