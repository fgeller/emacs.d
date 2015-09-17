; -*- coding:utf-8 -*-

;; don't gc during startup
(setq gc-cons-threshold 100000000)

(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp/org"))

;; temporarily ignore tramp and other regex'd handlers. got to love dyn scope.
(let ((file-name-handler-alist nil))
  (require 'org)
  (org-babel-load-file (expand-file-name "~/.emacs.d/main.org")))

;; reduce gc threshold to decrease gc pause
(setq gc-cons-threshold 1000000)
