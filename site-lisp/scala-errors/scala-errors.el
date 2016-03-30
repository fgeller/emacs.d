;;; scala-errors.el --- Quickly navigate to errors in a Scala project using sbt-quickfix.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016 Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>
;; Version: 0.1
;; Package-Requires: ((s "1.9.0") (f "0.16.0") (dash "2.5.0"))

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Quickly navigate to errors in a scala project. Requires the 'sbt-quickfix'
;; SBT plugin:
;;
;;   https://github.com/dscleaver/sbt-quickfix

;;; Code:

(require 'compile)
(require 'dash)
(require 'f)
(require 'rx)
(require 's)

(defgroup scala-errors nil
  "Quickly navigate to errors in a Scala project using sbt-quickfix."
  :group 'languages
  :prefix "scala-errors-")

(defcustom scala-errors-display-errors-buffer-function #'scala-errors-default-display-errors-function
  "Command used to display the compilation buffer.

It must be a unary function, taking the buffer to be displayed as an
argument."
  :group 'scala-errors
  :type 'function)



(defvar-local scala-errors--quickfix-last-update nil
  "Stores the last time the quickfix file was updated.
This is set after comparisons with the current mod date of the file on disk.")

(defvar scala-errors--file-polling-interval 0.2
  "The interval in seconds at which to poll for quickfix file creation.
Used when refreshing the error list.")

(defvar scala-errors--file-polling-max-time 5
  "The maximum time in seconds for polling for the quickfix file.
Used when refreshing the error list.")

(defconst scala-errors-keymap
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map t)
    (define-key map (kbd "g") #'scala-errors-refresh)
    map)
  "Keymap for scala-errors minor mode")

(defvar scala-errors--errors nil
  "Information about currently found errors.")

(defvar scala-errors--index nil
  "Index to currently visited error in `scala-errors--errors'.")



(defun scala-errors-default-display-errors-function (buf)
  (pop-to-buffer buf)
  (resize-temp-buffer-window (get-buffer-window buf))
  (message "Press 'g' to refresh errors if they get out-of-sync with SBT"))

;;;###autoload
(defun scala-errors-show-errors ()
  "Display SBT errors in a compilation buffer."
  (interactive)
  (-when-let (file (scala-errors--quickfix-file-path))
    (let ((buf (find-file-noselect file t)))
      (with-current-buffer buf
        (rename-buffer (scala-errors--buffer-name))
        (cond
         ((not (f-exists? (buffer-file-name)))
          (scala-errors-refresh))
         (t
          (scala-errors-mode)
          (goto-char (point-min)))))
      buf)))

;;;###autoload
(defun scala-errors-goto-first-error ()
  "Navigate to the first SBT error."
  (interactive)
  (let ((buf (scala-errors-show-errors)))
    (if (and buf (buffer-live-p buf))
        (scala-errors-goto-nth-error 0)
      (user-error "No errors"))))

(defun scala-errors-goto-nth-error (n)
  "Jump to a specific SBT error"
  (interactive "n")
  (if (< n (length scala-errors--errors))
      (let* ((info (nth n scala-errors--errors))
             (file (cdr (assoc 'file info)))
             (line (cdr (assoc 'line info)))
             (column (cdr (assoc 'column info)))
             (msg (cdr (assoc 'message info))))
        (find-file file)
        (goto-line line)
        (beginning-of-line)
        (forward-char column)
        (setq scala-errors--index n)
        (message "%s" msg))
    (user-error (format "Not enough errors to jump to error %s, only found %s." (1+ n) (length scala-errors--errors)))))

;;;###autoload
(defun scala-errors-goto-next-error ()
  "Navigate to the next SBT error."
  (interactive)
  (save-window-excursion (scala-errors-show-errors))
  (scala-errors-goto-nth-error (if scala-errors--index
                                   (1+ scala-errors--index)
                                 0)))

;;;###autoload
(defun scala-errors-goto-prev-error ()
  "Navigate to the previous SBT error."
  (interactive)
  (save-window-excursion (scala-errors-show-errors))
  (scala-errors-goto-nth-error (if scala-errors--index
                                   (1- scala-errors--index)
                                 0)))

;;;###autoload
(defun scala-errors-refresh ()
  "Delete the quickfix file and redisplay the errors list."
  (interactive)
  (scala-errors--delete-quickfix)
  (scala-errors--force-generate-quickfix)
  (run-with-timer scala-errors--file-polling-interval nil
                  #'scala-errors--poll-until-exists 0)
  (message "Refreshing SBT errors..."))

(defun scala-errors--poll-until-exists (repetitions)
  (let ((time-spent (* scala-errors--file-polling-interval repetitions)))
    (cond
     ((< scala-errors--file-polling-max-time time-spent)
      (message "No error output within %s seconds" scala-errors--file-polling-max-time)
      (message "No errors from SBT"))
     ((scala-errors--quickfix-file-path)
      (scala-errors-show-errors))
     (t
      (run-with-timer scala-errors--file-polling-interval
                      nil #'scala-errors--poll-until-exists (1+ repetitions))))))

(defun scala-errors--delete-quickfix ()
  (-when-let* ((buf (get-buffer (scala-errors--buffer-name)))
               (file (buffer-file-name buf)))
    (when (f-exists? file)
      (f-delete file))
    (-when-let (wins (get-buffer-window-list buf))
      (-each wins #'delete-window))

    (kill-buffer buf)))

(defun scala-errors--force-generate-quickfix ()
  (-when-let (last-buf (car (buffer-list)))
    (with-current-buffer last-buf
      (scala-errors--touch))))

(defun scala-errors--touch ()
  (insert " ")
  (backward-delete-char 1)
  (save-buffer))

(defun scala-errors--buffer-name ()
  (let ((x (format "*quickfix <%s>*" (scala-errors--project-name))))
    (message "found buffer name [%s]" x)
    x))



(defun scala-errors--project-root ()
  (or (locate-dominating-file default-directory "target")
      (locate-dominating-file default-directory "build.sbt")
      (locate-dominating-file default-directory ".git")
      (locate-dominating-file default-directory "src")))

(defun scala-errors--project-name ()
  (-last-item (f-split (scala-errors--project-root))))

(defun scala-errors--goto-first-compilation-link ()
  (goto-char (point-min))
  (while (not (or (eobp)
                  (get-text-property (point) 'compilation-message)))
    (forward-char 1)))

(defun scala-errors--quickfix-file-path ()
  (-when-let (proj-root (scala-errors--project-root))
    (message "found proj-root %s" proj-root)
    (let ((sbt (f-join proj-root "target/quickfix/sbt.quickfix"))
          (manual (f-join proj-root "quickfix"))) ;; TODO: customize list of options?
      (cond ((f-exists? sbt) sbt)
            ((f-exists? manual) manual)))))

(defvar scala-errors--error-re
  (rx bol "[error]" (+ space) (group (+ (not (any ":")))) ":" (group  (+ (not (any ":")))) ":" (* space) (group (+ nonl)) eol)
  "Regex to match an sbt error line.")

(defvar scala-errors--error-column-re
  (rx bol "[error]" (+ space) "^" eol)
  "Regex to match an sbt error column line.")

(defun scala-errors--find-errors ()
  (let* (errors)
    (save-excursion
      (goto-char (point-min))
      (message "looking for %s" scala-errors--error-re)
      (while (re-search-forward scala-errors--error-re (point-max) t)
        (let ((file (match-string 1))
              (line (string-to-number (match-string 2)))
              (msg (match-string 3))
              (column nil)
              (two-lines (save-excursion (forward-line 2) (end-of-line) (point))))
          (message "found error [%s] [%s] [%s]"  file line msg)
          (when (and scala-errors--error-column-re
                     two-lines
                     (re-search-forward scala-errors--error-column-re two-lines t))
            (setq column (- (1- (point)) (save-excursion (beginning-of-line) (point)) (length "[error] "))))
          (setq errors (cons `((file . ,file) (line . ,line) (message . ,msg) (column . ,column)) errors))))
    (setq scala-errors--errors (reverse errors)))))

(define-minor-mode scala-errors-mode "Scala Errors" nil "se" scala-errors-keymap
  (auto-revert-mode +1)
  (read-only-mode +1)
  (scala-errors--find-errors)
  (font-lock-add-keywords
   nil
   `(
     ;; Hide [error] level notes.
     (,(rx bol "[error]" (? space)) (0 '(face nil invisible t)))
     ;; Hide gvim call
     (,(rx bol (* nonl) "Cannot run program \"gvim\"" (* nonl) eol) (0 '(face nil invisible t)))
     ;; Highlight error column arrows
     (,(rx bol  (* space) (or "[error]" "[warn]") (+ space) (group "^") (* space) eol)
      (1 font-lock-constant-face))
     ;; Highlight error column arrows
     (,(rx bol  (* space) (or "[error]" "warn")
           (+ space) (group (+ alpha) space "error" (? "s") space "found") (* space) eol)
      (1 font-lock-comment-face))

     (,(rx (group "/" (+ (not (any ":"))) "/") (+ (not (any "/" ":"))) ":" (+ num) ":" (group (* nonl)))
      ;; Shorten paths
      (1 '(face nil invisible t))
      ;; Colour messages
      (2 font-lock-string-face)))))



;;;###autoload
(defun scala-errors-init ()
  (with-eval-after-load 'aggressive-indent
    (when (boundp 'aggressive-indent-excluded-modes)
      (add-to-list 'aggressive-indent-excluded-modes 'scala-errors-mode))))

;;;###autoload
(defun scala-errors-spacemacs-init ()
  (when (fboundp 'spacemacs/set-leader-keys-for-major-mode)
    (spacemacs/set-leader-keys-for-major-mode 'scala-mode
      "mfl" #'scala-errors-show-errors
      "mfg" #'scala-errors-refresh
      "mff" #'scala-errors-goto-first-error
      "mfn" #'scala-errors-goto-next-error
      "mfp" #'scala-errors-goto-prev-error)))

(provide 'scala-errors)

;;; scala-errors.el ends here

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
