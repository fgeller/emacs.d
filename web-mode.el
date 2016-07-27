(use-package web-mode :ensure web-mode
  :defer t
  :mode
  ("\\.html?\\'" . web-mode)
  ("\\.js\\'" . web-mode)
  :config
  (setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'")))
  (setq web-mode-enable-auto-closing nil)
  (add-hook 'web-mode-hook 'web-mode-customizations))

(defun web-mode-customizations ()
  (yas-minor-mode 1))


