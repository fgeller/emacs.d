; -*- coding:utf-8 -*-

(let (;; don't gc during startup
      (gc-cons-threshold 100000000)
      ;; temporarily ignore tramp and other regex'd handlers.
      (file-name-handler-alist nil))
  (add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp/org"))
  (require 'org)
  (org-babel-load-file (expand-file-name "~/.emacs.d/main.org")))
