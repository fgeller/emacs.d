(setq-default global-font-lock-mode nil)
(global-font-lock-mode -1)
(remove-hook 'occur-mode-hook 'turn-on-font-lock)

(use-package highlight-thing
  :init (global-highlight-thing-mode 1)
  :config (setq highlight-thing-what-thing 'symbol))

(tool-bar-mode -1)
(menu-bar-mode -1)  ;; shows full-screen button for mac port
(setq-default
 blink-cursor-delay 0.5
 blink-cursor-interval 1
 use-file-dialog nil
 use-dialog-box nil
 inhibit-startup-screen t
 inhibit-startup-echo-area-message t
 truncate-lines t
 truncate-partial-width-windows nil
 visible-bell 1
 transient-mark-mode t   ;; highlight the active region when mark is active
 show-trailing-whitespace t ;; don't show trailing whitespace globally
 blink-matching-paren t
 initial-frame-alist '((left-fringe . 1) (right-fringe . 1) (scroll-bar-width . 0) (vertical-scroll-bars . nil))
 default-frame-alist '((left-fringe . 1) (right-fringe . 1) (scroll-bar-width . 0) (vertical-scroll-bars . nil))
 scroll-bar-width 0
 default-frame-scroll-bars nil)

(defun mode-line-bell ()
  (let ((orig (face-attribute 'mode-line :background)))
    (set-face-attribute 'mode-line nil :background "red")
    (sit-for 0 70)
    (set-face-attribute 'mode-line nil :background orig)))

(setq
 visible-bell nil
 ring-bell-function 'mode-line-bell)

(defun set-default-face-attributes ()
  (interactive)
  (set-face-attribute 'default nil
                      :family mono-space-font-family
                      :height mono-space-font-height
                      :weight 'normal))

(setq mono-space-font-family "Roboto Mono")
(setq mono-space-font-height 140)
(set-default-face-attributes)

(defun set-font-height (height)
  (interactive
   (list (read-number "Height: " mono-space-font-height)))
  (setq mono-space-font-height height)
  (set-default-face-attributes))

(defun increase-font-height ()
   (interactive)
   (set-font-height (+ (face-attribute 'default :height) 10)))
(defun decrease-font-height ()
   (interactive)
   (set-font-height (- (face-attribute 'default :height) 10)))

(load-theme 'iv t)

(show-paren-mode 1)
(setq show-paren-delay 0)
(setq show-paren-style 'parenthesis)

(add-to-list 'display-buffer-alist
             `(,(rx bos "*Compile-" (* not-newline) "*" eos)
               (display-buffer-in-side-window)
               (inhibit-same-window . t)
               (window-height . 0.3)))

(use-package nlinum
  :ensure nlinum
  :commands linum-mode)

(setq ediff-split-window-function 'split-window-horizontally)

(setq ediff-window-setup-function 'ediff-setup-windows-plain)

(use-package leerzeichen :ensure leerzeichen)
