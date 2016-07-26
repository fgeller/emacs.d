
(use-package gtags)
(use-package helm-gtags)

(defun gtags-clean-root ()
  (interactive)
  (let* ((project-root (ag/project-root default-directory))
         (default-directory project-root))
    (mapcar (lambda (f)
              (let ((expanded-file-name (expand-file-name f)))
                (when (file-exists-p expanded-file-name)
                  (delete-file expanded-file-name))))
            '("GTAGS" "GRTAGS" "GPATH"))))

(defun gtags-reset-scala-root ()
  (interactive)
  (let* ((project-root (ag/project-root default-directory))
         (default-directory project-root))
    (let ((cmd "find . -name \"*.scala\" | while read f; do GTAGSLABEL=ctags GTAGSCONF=~/gtags.conf gtags -i --single-update $f; done"))
     (gtags-clean-root)
     (compile cmd))))

(defun gtags-update-single (filename gtags-root)
  "Update GNU Global database in GTAGS-ROOT for changes in file named FILENAME."
  (interactive)
  (let ((command (concat "cd " gtags-root " ; GTAGSLABEL=ctags GTAGSCONF=~/gtags.conf gtags -v --debug -i --single-update " filename )))
   (start-process "update-gtags" "update-gtags" "zsh" "-c" command)))

(defun gtags-update-current-file ()
  "Updates a GNU Global database based on the definitions in the current file."
  (interactive)
  (let* ((gtags-root (gtags-get-rootpath))
         (filename (buffer-file-name (current-buffer))))
    (gtags-update-single filename gtags-root)
    (message "Gtags updated for %s" filename)))

(defun gtags-update-hook ()
  "Optionally updates the GNU Global database incrementally, if applicable."
  (when (and (boundp 'gtags-mode) gtags-mode)
    (when (gtags-get-rootpath)
      (gtags-update-current-file))))

(defun initialize-gtags-mode () (add-hook 'after-save-hook 'gtags-update-hook))
(add-hook 'gtags-mode-hook 'initialize-gtags-mode)
