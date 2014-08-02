;;; dired-mode.el --- dired customization
;;; $Id$

;;; Commentary:
;;


;;; History:
;;

;;; Code:

;;; thanks to Robert Thorpe


(defun dired-follow-file ()
  "In dired, visit the file or directory on this line.
If a directory is on the current line, replace the current Dired buffer
with one containing the contents of the directory.  Otherwise, invoke
`dired-find-file' on the file."
  (interactive)
  (let ((filename (dired-get-filename)))
    (if (file-directory-p filename)
        (find-alternate-file filename)
      (dired-find-file))))

(defun arv-goto-parent-directory ()
  (interactive)
  (find-alternate-file ".."))

(eval-after-load "dired"
  '(progn
     (define-key dired-mode-map [(meta up)] 'arv-goto-parent-directory)
     (substitute-key-definition
      'dired-find-file 'dired-follow-file dired-mode-map)
     (substitute-key-definition
      'dired-up-directory 'arv-goto-parent-directory dired-mode-map)
     (substitute-key-definition
      'dired-advertised-find-file 'dired-follow-file dired-mode-map)))



;;; dired-mode.el ends here
