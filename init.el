; -*- coding:utf-8 -*-

(setq gc-cons-threshold 100000000)
(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp/org"))
(let ((file-name-handler-alist nil)) 
  (require 'org)
  (org-babel-load-file (expand-file-name "~/.emacs.d/main.org")))
(setq gc-cons-threshold 1000000)
