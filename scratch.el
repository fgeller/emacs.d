;; auto-save and auto-load scratch buffer
;; http://dorophone.blogspot.co.nz/2011/11/how-to-make-emacs-scratch-buffer.html

(defvar persistent-scratch-filename "~/.emacs.d/scratch" "Location of *scratch* file contents.")
(defvar persistent-scratch-backup-directory "~/.emacs.d/scratch-backups/" "Location of backups of the *scratch* buffer contents.")

(defun make-persistent-scratch-backup-name ()
  "Create a filename to backup the current scratch file by
  concatenating PERSISTENT-SCRATCH-BACKUP-DIRECTORY with the
  current date and time."
  (concat
   persistent-scratch-backup-directory
   (format-time-string "%Y-%m-%dT%H:%M:%S" (current-time))))

(defun save-persistent-scratch ()
  "Write the contents of *scratch* to the file name
  PERSISTENT-SCRATCH-FILENAME, making a backup copy in
  PERSISTENT-SCRATCH-BACKUP-DIRECTORY."
  (with-current-buffer (get-buffer "*scratch*")
    (if (file-exists-p persistent-scratch-filename)
        (copy-file persistent-scratch-filename
                   (make-persistent-scratch-backup-name)))
    (write-region (point-min) (point-max) persistent-scratch-filename)))

(defun load-persistent-scratch ()
  "Load the contents of PERSISTENT-SCRATCH-FILENAME into the
  scratch buffer, clearing its contents first."
  (if (file-exists-p persistent-scratch-filename)
      (with-current-buffer (get-buffer "*scratch*")
        (delete-region (point-min) (point-max))
	(insert-file-contents persistent-scratch-filename)
	(unless buffer-file-name
	  (set-visited-file-name persistent-scratch-filename)
	  (rename-buffer "*scratch*")
	  (org-mode)))))

(load-persistent-scratch)
(push #'save-persistent-scratch kill-emacs-hook)
