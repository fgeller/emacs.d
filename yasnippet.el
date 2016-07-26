(use-package yasnippet
  :ensure yasnippet
  :commands yas-minor-mode
  :config
  (setq yas-snippet-dirs (expand-file-name "~/.emacs.d/snippets"))
  (setq-default yas-prompt-functions (delete 'yas-x-prompt yas-prompt-functions))
  (yas-reload-all))
