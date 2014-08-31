;;; find-file-in-project.el --- inicialitzacio de ffip

;; $Id$


;;; Commentary:
;;


;;; History:
;;


;;; Code:


(eval-after-load "eproject"
  '(progn
     (setq ffip-project-root-function
           (lambda ()
             (if (boundp 'prj-directory)
                 prj-directory
               nil)))
     (setq ffip-project-file "eproject.cfg")))

(setq ffip-limit 1024)

;;; find-file-in-project.el ends here