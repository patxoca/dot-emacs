;;; find-file-in-project.el --- inicialitzacio de ffip

;; $Id$


;;; Commentary:
;;


;;; History:
;;


;;; Code:

(require 'find-file-in-project)
(require 'eproject)


(setq ffip-project-root-function
      (lambda ()
        (if (boundp 'prj-directory)
            prj-directory
          nil)))
(global-set-key (kbd "C-x f") 'find-file-in-project)
(setq ffip-project-file "eproject.cfg")
(setq ffip-limit 1024)

;;; find-file-in-project.el ends here
