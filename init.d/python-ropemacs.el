;;; python-ropemacs.el --- configuració per ropemacs

;; $Id$

;;; Commentary:
;;
;; Carrega i configura ropemacs si està disponible


;;; History:
;;


;;; Code:

(eval-after-load "pymacs"
  '(progn
     (message "Loading ropemacs ...")
     (condition-case ex
         (progn
           (pymacs-load "ropemacs" "rope-")
           (message "ropemacs loaded"))
       ('error (message "ropemacs failed")))))

;;; python-ropemacs.el ends here
