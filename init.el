;;; -*- coding:utf-8 -*-
(setq
 init-gc-cons-threshold gc-cons-threshold
 init-file-name-handler-alist file-name-handler-alist
 gc-cons-threshold 100000000
 file-name-handler-alist nil)

(setq mac-p (eq system-type 'darwin))
(setq linux-p (eq system-type 'gnu/linux))
(setq terminal-p (not window-system))
(when mac-p (setq system-name (car (split-string system-name "\\."))))

(setenv "EDITOR" "emacsclient")
(setenv "PAGER" "cat")

(setq custom-site-lisp-directory (expand-file-name "~/.emacs.d/site-lisp"))
(add-to-list 'load-path custom-site-lisp-directory)
(let ((default-directory custom-site-lisp-directory))
  (normal-top-level-add-subdirs-to-load-path))

(setq find-function-C-source-directory
      (expand-file-name (concat custom-site-lisp-directory "emacs-sources")))

(let ((themes-directory (expand-file-name (concat custom-site-lisp-directory "/themes"))))
  (mkdir themes-directory t)
  (mapcar (lambda (file)
            (let ((expanded-file (expand-file-name file themes-directory)))
              (when (file-directory-p expanded-file)
                (add-to-list 'custom-theme-load-path expanded-file))))
          (directory-files themes-directory)))

(setq-default custom-file (expand-file-name "~/.emacs.d/custom.el"))

(require 'cl)
(require 'package)
(require 'uniquify)

(setq package-user-dir (expand-file-name (concat custom-site-lisp-directory "/elpa")))
(setq package-archives '(("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)

(require 'use-package)

(use-package exec-path-from-shell
  :ensure exec-path-from-shell
  :init
  (when (memq window-system '(mac ns))
    (let ((exec-path-from-shell-variables '("PATH" "MANPATH" "GPG_AGENT_INFO" "SSH_AUTH_SOCK" "LANG")))
    (exec-path-from-shell-initialize))))

(setq auto-save-default nil)

(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)
(setq auto-revert-interval 0.1)
(auto-revert-set-timer)

(setq make-backup-files nil)

(defalias 'yes-or-no-p 'y-or-n-p)

(setq echo-keystrokes 0.01)

(setq scroll-step 1
      scroll-conservatively 10000)

(setq enable-local-variables :all)

(setq-default fill-column 80)

(column-number-mode 1)

(electric-indent-mode 1)
(electric-pair-mode 1)

(setq-default whitespace-style '(face tabs spaces trailing lines space-before-tab newline indentation::space empty space-after-tab space-mark tab-mark newline-mark))

(defun maybe-cleanup-whitespace ()
  (interactive)
  (when (and (boundp 'should-cleanup-whitespace)
             should-cleanup-whitespace)
    (whitespace-cleanup)))

(setq-default require-final-newline t)

(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")

(setq dired-dwim-target t)

(setq dired-listing-switches "-laGh")

(when linux-p
  (setq browse-url-browser-function 'browse-url-generic
        browse-url-generic-program "chromium-browser"))

(defun fold-left (fn acc seq)
  (let ((next (car seq))
        (rest (cdr seq)))
    (while next
      (setq acc (funcall fn acc next)
            next (car rest)
            rest (cdr rest))))
  acc)

(defun find-file-recursively (file-name dir)
  (let* ((default-directory dir)
         (contents (directory-files dir))
         (files (remove-if 'file-directory-p contents))
         (directories (remove-if-not (lambda (f) (and (file-directory-p f)
                                                      (not (string-equal "." f))
                                                      (not (string-equal ".." f))))
                               contents)))
    (if (member file-name files)
        (expand-file-name file-name dir)
      (fold-left (lambda (found nested-dir)
                   (if found
                       found
                     (find-file-recursively file-name (expand-file-name nested-dir dir))))
                 nil
                 directories))))

(load (expand-file-name "~/.emacs.d/ivy.el"))
(load (expand-file-name "~/.emacs.d/fingers.el"))
(load (expand-file-name "~/.emacs.d/appearance.el"))
(load (expand-file-name "~/.emacs.d/yasnippet.el"))
(load (expand-file-name "~/.emacs.d/org.el"))
(load (expand-file-name "~/.emacs.d/vc.el"))
(load (expand-file-name "~/.emacs.d/search.el"))
(load (expand-file-name "~/.emacs.d/auto-complete.el"))
(load (expand-file-name "~/.emacs.d/emacs-lisp.el"))
(load (expand-file-name "~/.emacs.d/scala.el"))
(load (expand-file-name "~/.emacs.d/java.el"))
(load (expand-file-name "~/.emacs.d/go.el"))
(load (expand-file-name "~/.emacs.d/web-mode.el"))
(load (expand-file-name "~/.emacs.d/email.el"))
(load (expand-file-name "~/.emacs.d/compilation.el"))
(load (expand-file-name "~/.emacs.d/javascript.el"))
(load (expand-file-name "~/.emacs.d/scratch.el"))

(setq
 gc-cons-threshold init-gc-cons-threshold
 file-name-handler-alist init-file-name-handler-alist)
