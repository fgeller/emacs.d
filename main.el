(setq mac-p (eq system-type 'darwin))
(setq linux-p (eq system-type 'gnu/linux))
(setq terminal-p (not window-system))
(when mac-p (setq system-name (car (split-string system-name "\\."))))

(setenv "EDITOR" "emacsclient")
(setenv "PAGER" "cat")

(setq custom-site-lisp-directory (expand-file-name "~/.emacs.d/site-lisp"))

(setq find-function-C-source-directory
      (expand-file-name (concat custom-site-lisp-directory "emacs-sources")))

(mapcar (lambda (addition)
          (add-to-list 'load-path addition)
          (let ((default-directory addition))
            (normal-top-level-add-subdirs-to-load-path)))
        `(,custom-site-lisp-directory))

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

(setq package-archives '(("melpa" . "http://melpa.milkbox.net/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")))

(package-initialize)
(require 'use-package)

(use-package exec-path-from-shell
  :ensure exec-path-from-shell
  :init (when (memq window-system '(mac ns))
          (exec-path-from-shell-initialize)
          (exec-path-from-shell-copy-env "GPG_AGENT_INFO")
          (exec-path-from-shell-copy-env "SSH_AUTH_SOCK")
          (exec-path-from-shell-copy-env "LANG")))

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

(electric-indent-mode -1)
(electric-pair-mode -1)

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

(server-start)

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

(defun show-hello-file-fullscreen ()
  (interactive)
  (let ((buf-name "*hello there*"))
    (unless (string= buf-name (buffer-name))
      (get-buffer-create buf-name)
      (with-current-buffer buf-name (insert "helo."))
      (view-buffer buf-name)
      (delete-other-windows))))

(run-with-idle-timer 120 t 'show-hello-file-fullscreen)

(load-file (expand-file-name "~/.emacs.d/ivy.el"))
(load-file (expand-file-name "~/.emacs.d/fingers.el"))
(load-file (expand-file-name "~/.emacs.d/appearance.el"))
(load-file (expand-file-name "~/.emacs.d/yasnippet.el"))
(load-file (expand-file-name "~/.emacs.d/org.el"))
(load-file (expand-file-name "~/.emacs.d/vc.el"))
(load-file (expand-file-name "~/.emacs.d/search.el"))
(load-file (expand-file-name "~/.emacs.d/auto-complete.el"))
(load-file (expand-file-name "~/.emacs.d/emacs-lisp.el"))
(load-file (expand-file-name "~/.emacs.d/eshell.el"))
(load-file (expand-file-name "~/.emacs.d/scala.el"))
(load-file (expand-file-name "~/.emacs.d/java.el"))
(load-file (expand-file-name "~/.emacs.d/go.el"))
(load-file (expand-file-name "~/.emacs.d/web-mode.el"))
(load-file (expand-file-name "~/.emacs.d/email.el"))
(load-file (expand-file-name "~/.emacs.d/compilation.el"))
(load-file (expand-file-name "~/.emacs.d/javascript.el"))
(load-file (expand-file-name "~/.emacs.d/scratch.el"))
