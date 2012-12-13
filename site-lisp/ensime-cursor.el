;; flymake-cursor for ensime
;;
;; http://www.emacswiki.org/emacs/FlymakeCursor
;; https://github.com/illusori/emacs-flymake-cursor

(defvar ec/e-at-point nil
  "Error at point, after last command")

(defvar ec/e-display-timer nil
  "A timer; when it fires, it displays the stored error message.")


(defun ec/show-stored-error-now ()
  "Displays the stored error in the minibuffer."
  (interactive)
  (let ((editing-p (= (minibuffer-depth) 0)))
   (if (and ec/e-at-point editing-p)
       (progn
         (message "%s" ec/e-at-point)
         (setq ec/e-display-timer nil)))))


(defun ec/get-error-at-point ()
  "Gets the first error on the line at point."
  (when (ensime-overlays-at (point))
    (overlay-get (car (ensime-overlays-at (point))) 'help-echo)))


;;;###autoload
(defun ec/show-error-at-point-now ()
  "If the cursor is sitting on an error, display the error
message in the minibuffer."
  (interactive)
  (if ec/e-display-timer
      (progn
        (cancel-timer ec/e-display-timer)
        (setq ec/e-display-timer nil)))
  (let ((error-at-point (ec/-get-error-at-point)))
    (if error-at-point
        (progn
          (setq ec/e-at-point error-at-point)
          (ec/show-stored-error-now)))))


;;;###autoload
(defun ec/show-error-at-point-pretty-soon ()
  "If the cursor is sitting on an error, grab the error,
and set a timer for \"pretty soon\". When the timer fires, the error
message will be displayed in the minibuffer.

This allows a post-command-hook to NOT cause the minibuffer to be
updated 10,000 times as a user scrolls through a buffer
quickly. Only when the user pauses on a line for more than a
second, does the error message (if any) get displayed.

"
  (interactive)
  (if ec/e-display-timer
      (cancel-timer ec/e-display-timer))

  (let ((error-at-point (ec/get-error-at-point)))
    (if error-at-point
        (setq ec/e-at-point error-at-point
              ec/e-display-timer
              (run-at-time "0.5 sec" nil 'ec/show-stored-error-now))
      (setq ec/e-at-point nil
            ec/e-display-timer nil))))


;;;###autoload
(eval-after-load "ensime"
  '(progn
     (defadvice ensime-mode (before ec/post-command-fn activate compile)
       "Add functionality to the post command hook so that if the
cursor is sitting on an error the error information is displayed
in the minibuffer (rather than having to mouse over it)"
       (set (make-local-variable 'post-command-hook)
            (cons 'ec/show-error-at-point-pretty-soon post-command-hook)))))


(provide 'ensime-cursor)
