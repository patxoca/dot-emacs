;;; python-ropemacs.el --- configuració per ropemacs

;; $Id$

;;; Commentary:
;;
;; Carrega i configura ropemacs si està disponible


;;; History:
;;

(require 'pymacs)

;;; Code:

(message "Loading ropemacs ...")
(condition-case ex
    (progn
      (pymacs-load "ropemacs" "rope-")
      (message "ropemacs loaded"))
  ('error (message "ropemacs failed")))

;;; python-ropemacs.el ends here
