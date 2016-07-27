(use-package ag :ensure ag :commands (ag ag-project))
(use-package wgrep :ensure wgrep :defer 3)
(use-package wgrep-ag :ensure wgrep-ag :defer 4)

(defun ag-project-with-thing-at-point ()
  (interactive)
  (let ((thing (thing-at-point 'symbol)))
    (ag-project thing)))
