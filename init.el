; -*- coding:utf-8 -*-

(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp/org"))
(require 'org)
(org-babel-load-file (expand-file-name "~/.emacs.d/main.org"))
