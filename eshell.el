(use-package eshell
  :defer t
  :init
  (setq eshell-where-to-jump 'begin
	eshell-review-quick-commands nil
	eshell-smart-space-goes-to-end t
	eshell-directory-name "~/.emacs.d/.eshell/"
	eshell-prompt-function #'eshell-simple-prompt-function
	eshell-prompt-regexp "\$ ")
  :config
  (add-to-list 'eshell-modules-list 'eshell-smart)
  (add-hook 'eshell-pre-command-hook  'eshell-rename-buffer-before-command)
  (add-hook 'eshell-post-command-hook 'eshell-rename-buffer-after-command)
  (add-hook 'eshell-mode-hook (lambda () (setq show-trailing-whitespace nil)))
  (add-hook 'eshell-mode-hook (lambda () (define-key eshell-mode-map (kbd "RET") 'eshell-send-input-customized)))
  )

(defun eshell-rename-buffer-before-command ()
  (let* ((last-input (buffer-substring eshell-last-input-start eshell-last-input-end)))
    (rename-buffer (format "*eshell[%s]$ %s...*" default-directory last-input) t)))

(defun eshell-rename-buffer-after-command ()
  (rename-buffer (format "*eshell[%s]$ %s*" default-directory (eshell-previous-input-string 0)) t))

(defun eshell-simple-prompt-function () "$ ")

(defun eshell/grt ()
  (interactive)
  (let ((root (locate-dominating-file default-directory ".git")))
    (eshell/cd root)))

(defun last-eshell ()
  (interactive)
  (let ((buf-name (cl-find-if (lambda (name) (string-match "*eshell\\[.+\\]\\$ " name))
                              (mapcar 'buffer-name (buffer-list)))))
    (if buf-name
        (pop-to-buffer-same-window buf-name)
      (eshell))))

(defun vc-deduce-backend ()
  (cond ((derived-mode-p 'vc-dir-mode)   vc-dir-backend)
        ((derived-mode-p 'log-view-mode) log-view-vc-backend)
        ((derived-mode-p 'log-edit-mode) log-edit-vc-backend)
        ((derived-mode-p 'diff-mode)     diff-vc-backend)
        ;; Maybe we could even use comint-mode rather than shell-mode?
        ((derived-mode-p 'dired-mode 'shell-mode 'eshell-mode 'compilation-mode)
         (vc-responsible-backend default-directory))
        (vc-mode (vc-backend buffer-file-name))))

(defun eshell-send-input-customized (&optional use-region queue-p no-newline)
  (interactive "*P")
  (if (and (thing-at-point 'word)
           (not (looking-at "\\>"))
           (string-match "[a-f0-9]\\{7,\\}" (thing-at-point 'word)))
      (let* ((vc-backend (vc-deduce-backend))
             (rev (thing-at-point 'word))
             (prev (vc-call-backend vc-backend 'previous-revision nil rev)))
        (vc-diff-internal t (list vc-backend) prev rev))
    (eshell-send-input use-region queue-p no-newline)))


(defun eshell/git-status (directory)
  (interactive)
  (eshell/cd directory)
  (insert "git status")
  (eshell-send-input))

(defun show-eshell-git-status ()
  (interactive)
  (let ((target-directory default-directory))
    (last-eshell)
    (eshell/git-status target-directory)))

(defun eshell/clear ()
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)))
