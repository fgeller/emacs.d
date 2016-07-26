; -*- coding:utf-8 -*-

; package.el forces this line to exist
; (package-initialize)

(let (;; don't gc during startup
      (gc-cons-threshold 100000000)
      ;; temporarily ignore tramp and other regex'd handlers.
      (file-name-handler-alist nil))
  (load-file (expand-file-name "~/.emacs.d/main.el")))
