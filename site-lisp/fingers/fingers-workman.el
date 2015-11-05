;;; fingers-workman.el --- Workman mapping for fingers.el

;; Copyright (c) 2015 Felix Geller

;; Author: Felix Geller <fgeller@gmail.com>
;; Keywords: fingers modal editing workman
;; URL: http://github.com/fgeller/fingers.el

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Sample mapping from Qwerty to Workman.
;;
;; Example for your configuration:
;;
;; (require 'fingers)
;; (require 'fingers-workman)
;; (setq fingers-keyboard-layout-mapper 'fingers-qwerty-to-workman)
;; (setq fingers-region-specifiers fingers-workman-region-specifiers)
;; (fingers-reset-bindings)

;;
;; Bindings are printed to the *Messages* buffer for debug info.
;;

(defvar fingers-qwerty-region-specifiers
  '((char . ?b)
    (char-and-whitespace . ?B)
    (line . ?G)
    (line-rest . ?g)
    (word . ?d)
    (word-and-whitespace . ?D)
    (symbol . ?f)
    (symbol-and-whitespace . ?F)
    (between-whitespace . ?v)
    (with-surrounding-whitespace . ?V)
    (inside-pair . ?s)
    (with-pair . ?a)
    (with-pair-and-whitespace . ?A))
  "Region specifiers tuned for the Qwerty layout.")

(defun fingers-qwerty-to-workman (keys)
  (let ((qwerty-to-workman
	 (lambda (char)
	   (let* ((should-upcase (and
				  (or (and (>= char 65) (<= char 90))
				      (and (>= char 97) (<= char 122)))
				  (= (upcase char) char)))
		  (lower-char (downcase char))
		  (workman-char (cond ((= ?a lower-char) "a")
				      ((= ?b lower-char) "v")
				      ((= ?c lower-char) "m")
				      ((= ?d lower-char) "h")
				      ((= ?e lower-char) "r")
				      ((= ?f lower-char) "t")
				      ((= ?g lower-char) "g")
				      ((= ?h lower-char) "y")
				      ((= ?i lower-char) "u")
				      ((= ?j lower-char) "n")
				      ((= ?k lower-char) "e")
				      ((= ?l lower-char) "o")
				      ((= ?m lower-char) "l")
				      ((= ?n lower-char) "k")
				      ((= ?o lower-char) "p")
				      ((= ?p lower-char) ";")
				      ((= ?q lower-char) "q")
				      ((= ?r lower-char) "w")
				      ((= ?s lower-char) "s")
				      ((= ?t lower-char) "b")
				      ((= ?u lower-char) "f")
				      ((= ?v lower-char) "c")
				      ((= ?w lower-char) "d")
				      ((= ?x lower-char) "x")
				      ((= ?y lower-char) "j")
				      ((= ?z lower-char) "z")
				      ((= ?\; lower-char) "i")
				      (t (string lower-char)))))
	     (cond ((and should-upcase (string= ";" workman-char)) ":")
		   (should-upcase (upcase workman-char))
		   (t workman-char))))))
    (cond ((string= "SPC" keys) keys)
	  ((string= "RET" keys) keys)
	  (t
	   (apply 'concat (mapcar qwerty-to-workman keys))))))

(provide 'fingers-workman)
