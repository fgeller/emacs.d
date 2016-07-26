(use-package js2-mode
  :ensure js2-mode
  :commands js2-mode
  :init (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  :config
  (add-hook 'js2-mode-hook (lambda () (font-lock-mode 1)))
  (setq js2-basic-offset 2))
