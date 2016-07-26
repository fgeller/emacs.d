(use-package elisp-slime-nav
  :ensure elisp-slime-nav
  :commands elisp-slime-nav-mode)

(defun emacs-lisp-customizations ()
  "My customizations for Emacs LISP buffers"
  (eldoc-mode 1)
  (yas-minor-mode 1)
  (elisp-slime-nav-mode 1))

(add-hook 'emacs-lisp-mode-hook 'emacs-lisp-customizations)
