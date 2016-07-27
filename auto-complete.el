(use-package company
  :defer 5
  :config
  (global-company-mode 1)
  (define-key company-mode-map (kbd "C-n") 'company-select-next)
  (define-key company-mode-map (kbd "C-p") 'company-select-previous))

