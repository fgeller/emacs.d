;;; fingers.el --- Modal editing with universal text manipulation helpers.

;; Copyright (c) 2014-2015 Felix Geller

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

;; fingers-mode is a global minor mode that introduces key bindings for
;; navigation and text manipulation. It relies on modal editing to reduce the
;; usage of modifiers like Control and Meta. It introduces a new keymap to
;; trigger commands without the need for modifiers and can easily be toggled to
;; fall back to inserting text as usually in Emacs.

;; More information: https://github.com/fgeller/fingers.el/

(require 'thingatpt)

;;
;; Helpers for bindings
;;

(defvar fingers-keyboard-layout-mapper 'identity "Mapping function from Workman to a different keyboard layout")
(defconst fingers-region-specifiers
  '((char . ?v)
    (char-and-whitespace . ?V)
    (line . ?G)
    (line-rest . ?g)
    (word . ?h)
    (word-and-whitespace . ?H)
    (symbol . ?t)
    (symbol-and-whitespace . ?T)
    (between-whitespace . ?c)
    (with-surrounding-whitespace . ?C)
    (inside-pair . ?s)
    (with-pair . ?a)
    (with-pair-and-whitespace . ?A))
  "Mapping from region type to identifier key")

(defun fingers-region-specifier (type)
  (cdr (assoc type fingers-region-specifiers)))

(defun fingers-pass-events (kbd-string)
  "Helper to pass keyboard events through to shadowed maps. Based on `boon-push-events'"
  (setq unread-command-events
        (append (kbd kbd-string) unread-command-events)))

(defun fingers-pass-events-command (k)
  (lexical-let ((kbd-string k)) ;; rebind so it isn't lost
    (defalias (intern (concat "fingers-pass-events-" (replace-regexp-in-string " " "_" kbd-string t t)))
      #'(lambda nil
          "Generated function to pass a keybard event (last part of the name) through via `fingers-pass-events'."
          (interactive)
          (fingers-pass-events kbd-string)))))

(defun fingers-clear-keymap (keymap)
  (let (loop)
    (setq loop 0)
    (while (<= loop ?z)
      (define-key map (char-to-string loop) nil)
      (setq loop (1+ loop)))))

(defun fingers-define-keys (layout-mapper map bindings)
  "Defines bindings in MAP as defined in BINDINGS"
  (fingers-clear-keymap map)
  (dolist (binding bindings)
    (let* ((key (cond ((symbolp (car binding)) (symbol-name (car binding)))
		      ((numberp (car binding)) (number-to-string (car binding)))
		      (t (error (format "unexpected key: %s" (car binding))))))
	   (target (cdr binding))
	   (mapped-sequence (funcall layout-mapper key)))
      (define-key map (kbd mapped-sequence) target))))

(defun fingers-meta ()
  (interactive)
  (let* ((next-key (read-char "M-"))
	 (next-key-sequence (concat "M-" (string next-key))))
    (fingers-pass-events next-key-sequence)))

(defun fingers-meta-control ()
  (interactive)
  (let* ((next-key (read-char "C-M-"))
	 (next-key-sequence (concat "C-M-" (string next-key))))
    (fingers-pass-events next-key-sequence)))

;;
;; Helpers for navigation
;;
(defun fingers-nav (cmd)
  (interactive)
  (let ((should-mark-p (not (and (not fingers-mark-active-from-jump-p)
				 mark-active))))
    (when (and should-mark-p mark-active)
      (deactivate-mark)
      (setq fingers-mark-active-from-jump-p nil))
    (if (string-match "^fingers-" (symbol-name cmd))
	(funcall cmd should-mark-p)
      (funcall cmd))))

(defmacro fingers-nav-command (name)
  `(lambda ()
     (interactive)
     (fingers-nav ',name)))

(defconst fingers-left-pair-start-regex
  "\\((\\|{\\|\\[\\|<\\|'\\|\"\\|`\\)"
  "Regex to identify the left start of a pair")

(defconst fingers-right-pair-end-regex
  "\\()\\|}\\|\\]\\|>\\|'\\|\"\\|`\\)"
  "Regex to identify the right end of a pair")

(defconst fingers-pairs
  '(("(" . ")")
    ("[" . "]")
    ("{" . "}")
    ("<" . ">")
    ("'" . "'")
    ("\"" . "\"")
    ("`" . "`"))
  "List of cons cells that are the start and end string of a pair")

(defvar fingers-mark-active-from-jump-p nil
  "Predicate to indicate that current mark is active from jump command")

(defun fingers-set-jump-mark ()
  "Set mark to trigger selection based on current navigation command"
  (setq fingers-mark-active-from-jump-p t)
  (set-mark (point)))

(defun fingers-beginning-of-line (should-mark-p)
  (interactive)
  (let ((start (point)))
    (beginning-of-line)
    (when (= start (point)) (back-to-indentation))))

(defun fingers-end-of-line (should-mark-p)
  (interactive)
  (end-of-line))

(defun fingers-mark-jump-char ()
  (save-excursion
    (forward-char)
    (fingers-set-jump-mark)))

(defun fingers-down (should-mark-p)
  (interactive)
  (next-line)
  ;; (when should-mark-p (fingers-mark-jump-char))
  )

(defun fingers-up (should-mark-p)
  (interactive)
  (previous-line)
  ;; (when should-mark-p (fingers-mark-jump-char))
  )

(defun fingers-right (should-mark-p)
  (interactive)
  (forward-char)
  ;; (when should-mark-p (fingers-mark-jump-char))
  )

(defun fingers-left (should-mark-p)
  (interactive)
  (backward-char)
  ;; (when should-mark-p (fingers-mark-jump-char))
  )

(defun fingers-end-of-buffer (should-mark-p)
  (interactive)
  (end-of-buffer))

(defun fingers-beginning-of-buffer (should-mark-p)
  (interactive)
  (beginning-of-buffer))

(defun fingers-backward (should-mark-p)
  (interactive)
  (let* ((next-symbol-start
	  (save-excursion
	    (forward-symbol -1)
	    (point)))
	 (next-pair-start
	  (save-excursion
	    (when (search-backward-regexp fingers-left-pair-start-regex (point-min) t)
	      (point)))))
    (if (and next-pair-start
	     (< next-symbol-start next-pair-start))
	(fingers-move-to-previous-pair-starter should-mark-p)
      (goto-char next-symbol-start)
      (when should-mark-p
	(save-excursion
	  (forward-symbol 1)
	  (fingers-set-jump-mark))))))

(defun fingers-char-at-point ()
  (buffer-substring (point) (1+ (point))))

(defun fingers-forward (should-mark-p)
  (interactive)
  (let* ((next-symbol-end
	  (save-excursion
	    (forward-symbol 1)
	    (point)))
	 (next-pair-end
	  (save-excursion
	    (when (search-forward-regexp fingers-right-pair-end-regex (point-max) t)
	      (point)))))
    (if (and next-pair-end
	     (> next-symbol-end next-pair-end))
	(fingers-move-to-next-pair-closer should-mark-p)
      (goto-char next-symbol-end)
      (when should-mark-p
	(save-excursion
	  (forward-symbol -1)
	  (fingers-set-jump-mark))))))

(defun fingers-move-to-previous-pair-starter (should-mark-p)
  (interactive)
  (let* ((first-pair-starter-pos
	  (save-excursion
	    (search-backward-regexp fingers-left-pair-start-regex (point-min) t)
	    (point)))
	 (second-pair-starter-pos
	  (save-excursion
	    (search-backward-regexp fingers-left-pair-start-regex (point-min) t 2)
	    (point)))
	 (first-pair-starter-str
	  (save-excursion
	    (goto-char first-pair-starter-pos)
	    (fingers-char-at-point))))
    (if (string-equal first-pair-starter-str (cdr (assoc first-pair-starter-str fingers-pairs)))
	(goto-char second-pair-starter-pos)
      (goto-char first-pair-starter-pos))
    (when should-mark-p
      (save-excursion
	(let* ((start (fingers-char-at-point))
	       (close (cdr (assoc start fingers-pairs))))
	  (fingers-move-point-to-pair-ending-string start close)
	  (forward-char 1)
	  (fingers-set-jump-mark))))))

(defun fingers-move-to-next-pair-closer (should-mark-p)
  (interactive)
  (let* ((first-pair-closer-pos
	  (save-excursion
	    (search-forward-regexp fingers-right-pair-end-regex (point-max) t)
	    (point)))
	 (first-pair-closer-str
	  (save-excursion
	    (goto-char (1- first-pair-closer-pos))
	    (fingers-char-at-point)))
	 (second-pair-closer-pos
	  (save-excursion
	    (search-forward-regexp fingers-right-pair-end-regex (point-max) t 2))))
    (if (string-equal first-pair-closer-str (car (rassoc first-pair-closer-str fingers-pairs)))
	(goto-char second-pair-closer-pos)
      (goto-char first-pair-closer-pos))
    (when should-mark-p
      (save-excursion
	(forward-char -1)
	(let* ((close (fingers-char-at-point))
	       (start (car (rassoc close fingers-pairs))))
	  (fingers-move-point-to-pair-starting-string start close)
	  (fingers-set-jump-mark))))))

(defun fingers-window-middle-line ()
  (let ((first-line (save-excursion (goto-char (window-start)) (line-number-at-pos)))
	(last-line (save-excursion (goto-char (window-end)) (line-number-at-pos))))
    (+ first-line (/ (- last-line first-line) 2))))

(defun fingers-page-down (should-mark-p)
  (interactive)
  (scroll-up-command 5)
  (goto-line (fingers-window-middle-line))
  (beginning-of-line)
  (when should-mark-p
    (save-excursion
      (end-of-line)
      (fingers-set-jump-mark))))

(defun fingers-page-up (should-mark-p)
  (interactive)
  (scroll-down-command 5)
  (goto-line (fingers-window-middle-line))
  (beginning-of-line)
  (when should-mark-p
    (save-excursion
      (end-of-line)
      (fingers-set-jump-mark))))

(defun fingers-move-to-next-word-occurrence (should-mark-p)
  (interactive)
  (fingers-beginning-of-word)
  (forward-word)
  (let ((thing (thing-at-point 'word)))
    (setq isearch-string thing)
    (search-forward-regexp (concat "\\<" (regexp-quote thing) "\\>")))
  (fingers-beginning-of-word)
  (when should-mark-p
    (save-excursion
      (forward-word)
      (fingers-set-jump-mark))))

(defun fingers-move-to-next-symbol-occurrence (should-mark-p)
  (interactive)
  (fingers-beginning-of-symbol)
  (forward-symbol 1)
  (let ((thing (thing-at-point 'symbol)))
    (setq isearch-string thing)
    (search-forward-regexp (concat "\\_<" (regexp-quote thing) "\\_>")))
  (fingers-beginning-of-symbol)
  (when should-mark-p
    (save-excursion
      (forward-symbol 1)
      (fingers-set-jump-mark))))

(defun fingers-move-to-previous-word-occurrence (should-mark-p)
  (interactive)
  (fingers-beginning-of-word)
  (let ((thing (thing-at-point 'word)))
    (setq isearch-string thing)
    (search-backward-regexp (concat "\\<" (regexp-quote thing) "\\>"))
    (when should-mark-p
      (save-excursion
	(forward-word)
	(fingers-set-jump-mark)))))

(defun fingers-move-to-previous-symbol-occurrence (should-mark-p)
  (interactive)
  (fingers-beginning-of-symbol)
  (let ((thing (thing-at-point 'symbol)))
    (setq isearch-string thing)
    (search-backward-regexp (concat "\\_<" (regexp-quote thing) "\\_>"))
    (when should-mark-p
      (save-excursion
	(forward-symbol 1)
	(fingers-set-jump-mark)))))

;;
;; Helpers for manipulation
;;
(defun fingers-undo (&optional arg)
  (interactive)
  (when (and fingers-mark-active-from-jump-p mark-active)
    (deactivate-mark))
  (undo arg))

(defun fingers-redo (&optional arg)
  (interactive)
  (when (and fingers-mark-active-from-jump-p mark-active)
    (deactivate-mark))
  (redo arg))

(defun fingers-open-line-below ()
  (interactive)
  (end-of-line)
  (open-line 1)
  (forward-line 1)
  (indent-for-tab-command)
  (global-fingers-mode -1))

(defun fingers-insert-char ()
  (interactive)
  (let ((char-to-insert (read-char "Insert: ")))
    (insert char-to-insert)))

(defun fingers-insert-sequence ()
  (interactive)
  (let ((str-to-insert (read-string "Insert: ")))
    (insert str-to-insert)))

(defun fingers-replace-with-char ()
  (interactive)
  (let ((char-to-insert (read-char "Replace with: ")))
    (if (region-active-p)
	(fingers-delete-region)
      (delete-char 1))
    (insert char-to-insert)
    (backward-char 1)))

(defun fingers-replace-with-yank ()
  (interactive)
  (if (region-active-p)
      (fingers-delete-region)
    (delete-char 1))
  (yank))

(defun fingers-beginning-of-line-and-insert ()
  (interactive)
  (fingers-beginning-of-line nil)
  (global-fingers-mode -1))

(defun fingers-end-of-line-and-insert ()
  (interactive)
  (fingers-end-of-line nil)
  (global-fingers-mode -1))

(defun fingers-open-above-and-insert ()
  (interactive)
  (beginning-of-line)
  (open-line 1)
  (fingers-mode -1))

(defun fingers-open-below-and-insert ()
  (interactive)
  (end-of-line)
  (open-line 1)
  (forward-line 1)
  (end-of-line)
  (fingers-mode -1))

(defun fingers-forward-delete ()
  (interactive)
  (delete-char 1))

(defun fingers-backward-delete ()
  (interactive)
  (delete-char -1))

(defun fingers-delete-region ()
  (delete-region (point) (mark)))

(defun fingers-copy-current-region (&optional kill)
  (cond (kill (kill-region (point) (mark)))
	(t (kill-ring-save (point) (mark)))))

(defun fingers-duplicate-line ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (set-mark (point))
    (end-of-line)
    (kill-ring-save (point) (mark))
    (open-line 1)
    (forward-char 1)
    (yank)))

(defun fingers-find-next-left-pair-start ()
  (save-excursion
    (search-backward-regexp "\\((\\|{\\|\\[\\|<\\|'\\|\"\\|`\\)" (point-min) t)
    (let ((start-char (buffer-substring-no-properties (point) (1+ (point)))))
      (cond ((string= "(" start-char) '("(" . ")"))
	    ((string= "[" start-char) '("[" . "]"))
	    ((string= "{" start-char) '("{" . "}"))
	    ((string= "<" start-char) '("<" . ">"))
	    ((string= "'" start-char) '("'" . "'"))
	    ((string= "\"" start-char) '("\"" . "\""))
	    ((string= "`" start-char) '("`" . "`"))))))

(defun fingers-dispatch-with-pair (target &optional default)
  (let* ((last-key-seq (this-single-command-keys))
	 (last-key (elt last-key-seq (1- (length last-key-seq))))
	 (next-key (read-char "Pair start character: "))
	 (inner-most-pair (fingers-find-next-left-pair-start)))
    (cond ((= next-key ?\() (funcall target "(" ")"))
          ((= next-key ?\{) (funcall target "{" "}"))
          ((= next-key ?\[) (funcall target "[" "]"))
          ((= next-key ?\<) (funcall target "<" ">"))
          ((= next-key ?\') (funcall target "'" "'"))
          ((= next-key ?\") (funcall target "\"" "\""))
	  ((= next-key ?\`) (funcall target "`" "`"))
	  ((and (= next-key last-key) inner-most-pair)
	   (funcall target (car inner-most-pair) (cdr inner-most-pair)))
          (t
	   (when default (funcall default))
	   (fingers-pass-events (string next-key))))))

(defun fingers-move-point-to-balanced-start (start end)
  (fingers-move-point-to-balanced t start end))

(defun fingers-move-point-to-balanced-end (start end)
  (fingers-move-point-to-balanced nil start end))

(defun fingers-move-point-to-balanced (look-for-start start end)
  (let ((counter 1))
    (while (> counter 0)
      (if look-for-start (backward-char 1) (forward-char 1))
      (cond ((looking-at (regexp-quote (if look-for-start end start))) (setq counter (1+ counter)))
            ((looking-at (regexp-quote (if look-for-start start end))) (setq counter (1- counter)))))))

(defun fingers-move-point-to-pair-start-simple (pair)
  (backward-char 1)
  (while (not (looking-at (regexp-quote pair)))
    (backward-char 1)))

(defun fingers-move-point-to-pair-end-simple (pair)
  (forward-char 1)
  (while (not (looking-at (regexp-quote pair)))
    (forward-char 1)))

(defun fingers-move-point-to-pair-starting-string (start end)
  (if (string= start end)
      (fingers-move-point-to-pair-start-simple start)
    (fingers-move-point-to-balanced-start start end)))

(defun fingers-move-point-to-pair-ending-string (start end)
  (if (string= start end)
      (fingers-move-point-to-pair-end-simple start)
    (fingers-move-point-to-balanced-end start end)))

(defun fingers-looking-at-symbol-p ()
  (looking-at "\\_<"))

(defun fingers-beginning-of-symbol ()
  (while (not (fingers-looking-at-symbol-p))
    (left-char 1)))

(defun fingers-looking-at-word-p ()
  (looking-at "\\<"))

(defun fingers-beginning-of-word ()
  (while (not (fingers-looking-at-word-p))
    (left-char 1)))

(defun fingers-set-mark-before-whitespace-and-return ()
  (let ((start-position (point)))
    (fingers-skip-whitespace-backward)
    (set-mark (point))
    (goto-char start-position)))

(defun fingers-skip-whitespace-forward ()
  (skip-chars-forward " \t\n"))

(defun fingers-skip-whitespace-backward ()
  (skip-chars-backward " \t\n"))

(defun fingers-increment-integer-at-point (&optional increment)
  (interactive "p*")
  (fingers-update-integer-at-point (lambda (num) (+ num (if increment increment 1)))))

(defun fingers-decrement-integer-at-point (&optional decrement)
  (interactive "p*")
  (fingers-update-integer-at-point (lambda (num) (- num (if decrement decrement 1)))))

(defun fingers-update-integer-at-point (update)
  (let ((offset (skip-chars-backward "0123456789")))
    (if (looking-at "[[:digit:]]+")
	(let* ((number-string (save-excursion
				(re-search-forward "[[:digit:]]+")
				(match-string 0)))
	       (should-pad-p (string-match "0+[[:digit:]]+" number-string))
	       (pad-number #'(lambda (num) (format (concat "%0" (number-to-string (length number-string)) "d") num)))
	       (number (string-to-number number-string))
	       (new-number (funcall update number))
	       (final-string (if should-pad-p (funcall pad-number new-number) (number-to-string new-number))))
	  (delete-region (point) (+ (point) (length number-string)))
	  (insert final-string)
	  (backward-char (+ (length number-string) offset)))
      (message "Can't identify number at point."))))

;;
;; mark
;;

(defun fingers-mark ()
  (interactive)
  (when mark-active (deactivate-mark))
  (setq fingers-mark-active-from-jump-p nil)
  (let ((next-key (read-char "Mark: ")))
    (cond
     ((= next-key (fingers-region-specifier 'char)) (fingers-mark-char))
     ((= next-key (fingers-region-specifier 'char-and-whitespace)) (fingers-mark-char-and-whitespace))
     ((= next-key (fingers-region-specifier 'line)) (fingers-mark-whole-line))
     ((= next-key (fingers-region-specifier 'line-rest)) (fingers-mark-until-end-of-line))
     ((= next-key (fingers-region-specifier 'word)) (fingers-mark-word))
     ((= next-key (fingers-region-specifier 'word-and-whitespace)) (fingers-mark-word-and-whitespace))
     ((= next-key (fingers-region-specifier 'symbol)) (fingers-mark-symbol))
     ((= next-key (fingers-region-specifier 'symbol-and-whitespace)) (fingers-mark-symbol-and-whitespace))
     ((= next-key (fingers-region-specifier 'between-whitespace)) (fingers-mark-between-whitespace))
     ((= next-key (fingers-region-specifier 'with-surrounding-whitespace)) (fingers-mark-with-surrounding-whitespace))
     ((= next-key (fingers-region-specifier 'inside-pair)) (fingers-mark-inside-pair))
     ((= next-key (fingers-region-specifier 'with-pair)) (fingers-mark-with-pair))
     ((= next-key (fingers-region-specifier 'with-pair-and-whitespace)) (fingers-mark-with-pair-and-whitespace))
     (t (set-mark (point))
	(fingers-pass-events (string next-key))))))

(defun fingers-mark-char ()
  (set-mark (point))
  (forward-char 1))

(defun fingers-mark-char-and-whitespace ()
  (fingers-set-mark-before-whitespace-and-return)
  (forward-char 1)
  (fingers-skip-whitespace-forward))

(defun fingers-mark-word ()
  (unless (fingers-looking-at-word-p) (fingers-beginning-of-word))
  (set-mark (point))
  (forward-word))

(defun fingers-mark-word-and-whitespace ()
  (unless (fingers-looking-at-word-p) (fingers-beginning-of-word))
  (fingers-set-mark-before-whitespace-and-return)
  (forward-word)
  (fingers-skip-whitespace-forward))

(defun fingers-mark-symbol ()
  (unless (fingers-looking-at-symbol-p) (fingers-beginning-of-symbol))
  (set-mark (point))
  (forward-symbol 1))

(defun fingers-mark-symbol-and-whitespace ()
  (unless (fingers-looking-at-symbol-p) (fingers-beginning-of-symbol))
  (fingers-set-mark-before-whitespace-and-return)
  (forward-symbol 1)
  (fingers-skip-whitespace-forward))

(defun fingers-mark-between-whitespace ()
  (search-backward-regexp "[ \t\n]" (point-min) t)
  (fingers-skip-whitespace-forward)
  (set-mark (point))
  (search-forward-regexp "[ \t\n]" (point-max) t)
  (fingers-skip-whitespace-backward))

(defun fingers-mark-with-surrounding-whitespace ()
  (search-backward-regexp "[ \t\n]" (point-min) t)
  (let ((non-whitespace-position (save-excursion
				   (fingers-skip-whitespace-forward)
				   (point))))
    (fingers-skip-whitespace-backward)
    (set-mark (point))
    (goto-char non-whitespace-position)
    (search-forward-regexp "[ \t\n]" (point-max) t)
    (fingers-skip-whitespace-forward)))

(defun fingers-mark-until-end-of-line ()
  (set-mark (point))
  (end-of-line))

(defun fingers-mark-whole-line ()
  (beginning-of-line)
  (set-mark (point))
  (end-of-line))

(defun fingers-mark-inside-pair ()
  (fingers-dispatch-with-pair 'fingers-mark-inside-pair-strings
                              (lambda () (set-mark (point)))))

(defun fingers-mark-inside-pair-strings (start end)
  (fingers-move-point-to-pair-starting-string start end)
  (forward-char 1)
  (set-mark (point))
  (backward-char 1)
  (fingers-move-point-to-pair-ending-string start end))

(defun fingers-mark-with-pair ()
  (fingers-dispatch-with-pair 'fingers-mark-with-pair-strings))

(defun fingers-mark-with-pair-strings (start end)
  (fingers-move-point-to-pair-starting-string start end)
  (set-mark (point))
  (fingers-move-point-to-pair-ending-string start end)
  (forward-char 1))

(defun fingers-mark-with-pair-strings-and-whitespace (start end)
  (fingers-move-point-to-pair-starting-string start end)
  (let ((starting-position (point)))
    (fingers-skip-whitespace-backward)
    (set-mark (point))
    (goto-char starting-position))
  (fingers-move-point-to-pair-ending-string start end)
  (forward-char 1)
  (fingers-skip-whitespace-forward))

(defun fingers-mark-with-pair-and-whitespace ()
  (fingers-dispatch-with-pair 'fingers-mark-with-pair-strings-and-whitespace))

;;
;; kill & copy
;;

(defun fingers-kill ()
  (interactive)
  (fingers-copy 'kill))

(defun fingers-copy (&optional kill)
  (interactive)
  (cond ((region-active-p) (fingers-copy-current-region kill))
	(t (let ((next-key (read-char "Kill: ")))
	     (cond
	      ((= next-key (fingers-region-specifier 'char)) (fingers-copy-char kill))
	      ((= next-key (fingers-region-specifier 'char-and-whitespace)) (fingers-copy-char-and-whitespace kill))
	      ((= next-key (fingers-region-specifier 'line)) (fingers-copy-whole-line kill))
	      ((= next-key (fingers-region-specifier 'line-rest)) (fingers-copy-until-end-of-line kill))
	      ((= next-key (fingers-region-specifier 'word)) (fingers-copy-word kill))
	      ((= next-key (fingers-region-specifier 'word-and-whitespace)) (fingers-copy-word-and-whitespace kill))
	      ((= next-key (fingers-region-specifier 'symbol)) (fingers-copy-symbol kill))
	      ((= next-key (fingers-region-specifier 'symbol-and-whitespace)) (fingers-copy-symbol-and-whitespace kill))
	      ((= next-key (fingers-region-specifier 'between-whitespace)) (fingers-copy-between-whitespace kill))
	      ((= next-key (fingers-region-specifier 'with-surrounding-whitespace)) (fingers-copy-with-surrounding-whitespace kill))
	      ((= next-key (fingers-region-specifier 'inside-pair)) (fingers-copy-inside-pair kill))
	      ((= next-key (fingers-region-specifier 'with-pair)) (fingers-copy-with-pair kill))
	      ((= next-key (fingers-region-specifier 'with-pair-and-whitespace)) (fingers-copy-with-pair-and-whitespace kill))
	      (t (set-mark (point))
		 (call-interactively (key-binding (kbd (string next-key))))
		 (fingers-copy-current-region kill)))))))

(defun fingers-copy-char (&optional kill)
  (fingers-mark-char)
  (fingers-copy-current-region kill))

(defun fingers-copy-char-and-whitespace (&optional kill)
  (fingers-mark-char-and-whitespace)
  (fingers-copy-current-region kill))

(defun fingers-copy-word (&optional kill)
  (fingers-mark-word)
  (fingers-copy-current-region kill))

(defun fingers-copy-word-and-whitespace (&optional kill)
  (fingers-mark-word-and-whitespace)
  (fingers-copy-current-region kill))

(defun fingers-copy-symbol (&optional kill)
  (fingers-mark-symbol)
  (fingers-copy-current-region kill))

(defun fingers-copy-symbol-and-whitespace (&optional kill)
  (fingers-mark-symbol-and-whitespace)
  (fingers-copy-current-region kill))

(defun fingers-copy-between-whitespace (&optional kill)
  (fingers-mark-between-whitespace)
  (fingers-copy-current-region kill))

(defun fingers-copy-with-surrounding-whitespace (&optional kill)
  (fingers-mark-with-surrounding-whitespace)
  (fingers-copy-current-region kill))

(defun fingers-copy-until-end-of-line (&optional kill)
  (fingers-mark-until-end-of-line)
  (fingers-copy-current-region kill))

(defun fingers-copy-whole-line (&optional kill)
  (fingers-mark-whole-line)
  (fingers-copy-current-region kill)
  (delete-char 1))

(defun fingers-copy-inside-pair (&optional kill)
  (fingers-mark-inside-pair)
  (fingers-copy-current-region kill))

(defun fingers-copy-with-pair (&optional kill)
  (fingers-mark-with-pair)
  (fingers-copy-current-region kill))

(defun fingers-copy-with-pair-and-whitespace (&optional kill)
  (fingers-mark-with-pair-and-whitespace)
  (fingers-copy-current-region kill))

;;
;; enclose
;;

(defun fingers-enclose-in-pair ()
  (interactive)
  (unless (region-active-p) (fingers-mark))
  (fingers-dispatch-with-pair 'fingers-enclose-in-pair-strings))

(defun fingers-enclose-in-pair-strings (start end)
  (let* ((mark-position (mark))
         (point-position (point))
         (start-position (min mark-position point-position))
         (end-position (max mark-position point-position)))
    (goto-char end-position)
    (insert end)
    (goto-char start-position)
    (insert start)
    (goto-char (+ end-position (length end)))))

;;
;; remove enclosing pair
;;

(defun fingers-remove-enclosing-pair ()
  (interactive)
  (fingers-dispatch-with-pair 'fingers-remove-enclosing-pair-strings))

(defun fingers-remove-enclosing-pair-strings (start end)
  (fingers-mark-inside-pair-strings start end)
  (let ((start-position (mark)))
    (delete-char (length end))
    (goto-char start-position)
    (delete-char (- (length start)))))

;;
;; Keymaps
;;

(defun fingers-mode-clean-map ()
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map t)
    map))

(defvar fingers-mode-map (fingers-mode-clean-map))
(defvar fingers-mode-x-map (fingers-mode-clean-map))
(defvar fingers-mode-c-map (fingers-mode-clean-map))
(defvar fingers-mode-toggle-map (fingers-mode-clean-map))
(defvar fingers-mode-launch-map (fingers-mode-clean-map))

(defconst fingers-command-bindings
  `(
    ;; left hand -- manipulation
    ;;
    ;; q d r w b
    ;; a s h t g
    ;; z x m c v

    ;; top row
    (q . nil)
    (Q . nil)
    (d . fingers-forward-delete)
    (D . fingers-backward-delete)
    (r . fingers-replace-with-char)
    (R . fingers-replace-with-yank)
    (w . fingers-insert-char)
    (W . nil)
    (b . fingers-insert-sequence)
    (B . nil)

    ;; home row
    (a . fingers-enclose-in-pair)
    (A . nil)
    (s . fingers-remove-enclosing-pair)
    (S . nil)
    (h . yank)
    (H . yank-pop)
    (t . fingers-kill)
    (T . fingers-copy)
    (g . fingers-meta)
    (G . fingers-meta-control)

    ;; bottom row
    (z . fingers-eol-and-insert)
    (Z . fingers-bol-and-insert)
    (x . ,fingers-mode-x-map)
    (X . query-replace-regexp)
    (m . kmacro-start-macro)
    (M . kmacro-end-macro)
    (c . ,fingers-mode-c-map)
    (C . fingers-duplicate-line)
    (V . join-line)
    (v . fingers-open-line-below)

    ;; right hand -- navigation
    ;;
    ;; j f u p ; [
    ;; y n e o i '
    ;; k l , . /

    (,(intern "`")	. goto-line)

    ;; top row
    (j		. ,(fingers-nav-command fingers-beginning-of-buffer))
    (J		. ,(fingers-nav-command fingers-end-of-buffer))
    (f		. nil)
    (u		. isearch-forward)
    (,(intern "}")	. ,(fingers-nav-command fingers-move-to-next-word-occurrence))
    (,(intern "]")	. ,(fingers-nav-command fingers-move-to-next-symbol-occurrence))
    (,(intern "{")	. ,(fingers-nav-command fingers-move-to-previous-word-occurrence))
    (,(intern "[")	. ,(fingers-nav-command fingers-move-to-previous-symbol-occurrence))
    (,(intern ";")	. pop-to-mark-command)
    (,(intern ":")	. pop-global-mark)

    ;; home row
    (y		. ,(fingers-nav-command fingers-beginning-of-line))
    (Y		. ,(fingers-nav-command fingers-move-to-previous-pair-starter))
    (n		. ,(fingers-nav-command fingers-left))
    (N		. ,(fingers-nav-command fingers-backward))
    (e		. ,(fingers-nav-command fingers-down))
    (E		. ,(fingers-nav-command fingers-page-down))
    (o		. ,(fingers-nav-command fingers-up))
    (O		. ,(fingers-nav-command fingers-page-up))
    (i		. ,(fingers-nav-command fingers-right))
    (I		. ,(fingers-nav-command fingers-forward))
    (,(intern "'")	. ,(fingers-nav-command fingers-end-of-line))
    (,(intern "\"")	. ,(fingers-nav-command fingers-move-to-next-pair-closer))

    ;; bottom row
    (k . grep)
    (/ . fingers-undo)
    (? . fingers-redo)

    (SPC . fingers-mark)
    (+ . fingers-increment-integer-at-point)
    (- . fingers-decrement-integer-at-point)
    )
  "Main bindings in `fingers-mode-map'")

(defconst fingers-x-bindings
  `(
    (b . switch-to-buffer)
    (c . save-buffers-kill-terminal)
    (e . eval-last-sexp)
    (f . find-file)
    (h . mark-whole-buffer)
    (k . kill-buffer)
    (K . ,(lambda () (interactive) (kill-buffer nil)))
    (l . ,fingers-mode-launch-map)
    (m . message-mail)
    (o . other-window)
    (s . ,(fingers-pass-events-command "C-x C-s"))
    (S . save-some-buffers)
    (t . ,fingers-mode-toggle-map)
    (vd . vc-diff)
    (vD . vc-root-diff)
    (ve . vc-version-ediff)
    (vg . vc-annotate)
    (vl . vc-print-log)
    (vu . vc-revert)
    (v~ . vc-revision-other-window)
    (x . execute-extended-command)
    (0 . delete-window)
    (1 . delete-other-windows)
    (2 . split-window-below)
    (3 . split-window-right)
    (50 . delete-frame)
    (52 . make-frame-command)
    (,(intern "#") . server-edit)
    )
  "Bindings for `fingers-mode-x-map'")

(defconst fingers-c-bindings
  `(
    ,@(mapcar (lambda (sym)
                `(,sym . ,(fingers-pass-events-command (concat "C-c C-" (symbol-name sym)))))
              `(a b c d e f g h i j k l m n o p q r s t u v w y z))

    (xa . ,(fingers-pass-events-command "C-c C-x C-a"))
    (,(intern "'") . ,(fingers-pass-events-command "C-c '"))
    (,(intern "!") . ,(fingers-pass-events-command "C-c !"))
    (,(intern "/") . ,(fingers-pass-events-command "C-c /"))
    (RET . ,(fingers-pass-events-command "C-c RET"))
    )
  "Bindings for `fingers-mode-c-map'")

(defconst fingers-launch-bindings
  `(
    (C . compile)
    (c . recompile)
    (d . ediff-buffers)
    (e . eshell)
    (m . man)
    (oa . org-agenda)
    (oc . org-capture)
    (ol . org-store-link)
    (oki . org-clock-in)
    (oko . org-clock-out)
    (okj . org-clock-jump-to-current-clock)
    )
  "Bindings for `fingers-mode-launch-map'")

(defconst fingers-toggle-bindings
  `(
    (l . toggle-truncate-lines)
    (d . toggle-debug-on-error)
    (w . whitespace-mode)
    )
  "Bindings for `fingers-mode-toggle-map'")

;;
;; Main command mode map
;;
(defun fingers-reset-bindings ()
  (fingers-define-keys fingers-keyboard-layout-mapper
		       fingers-mode-map
		       fingers-command-bindings)
  (fingers-define-keys 'identity
		       fingers-mode-x-map
		       fingers-x-bindings)
  (fingers-define-keys 'identity
		       fingers-mode-launch-map
		       fingers-launch-bindings)
  (fingers-define-keys 'identity
		       fingers-mode-toggle-map
		       fingers-toggle-bindings)
  (fingers-define-keys 'identity
		       fingers-mode-c-map
		       fingers-c-bindings)
  (run-hooks 'fingers-after-reset-hook))

(fingers-reset-bindings)

;;
;; Mode management
;;
(defvar fingers-mode-excluded-major-modes '()
  "List of major-modes for which fingers-mode should not be activated.")

(defun fingers-mode-maybe-activate ()
  (unless (or (minibufferp)
	      (member major-mode fingers-mode-excluded-major-modes))
    (fingers-mode 1)))

;;;###autoload
(define-minor-mode fingers-mode
  "Minor mode that introduces key bindings for navigation and
text manipulation. It relies on modal editing to reduce the usage
of modifiers like Control and Meta. It introduces a new keymap to
trigger commands without the need for modifiers and can easily be
toggled to fall back to inserting text as usually in Emacs.

Available bindings:

\\{fingers-mode-map}
"
  nil " fingers" fingers-mode-map :group 'fingers)

;;;###autoload
(define-globalized-minor-mode global-fingers-mode
  fingers-mode
  fingers-mode-maybe-activate
  :group 'fingers)

(provide 'fingers)

;;; fingers.el ends here
