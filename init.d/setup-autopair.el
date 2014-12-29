;;; autopair.el --- configuracio de autopair
;;; $Id$

;;; Commentary:
;;


;;; History:
;;


;;; Code:


(if (or (and (= emacs-major-version 24)
             (>= emacs-minor-version 4))
        (> emacs-major-version 24))
    (electric-pair-mode 1)
  (autopair-global-mode 1))



;;; autopair.el ends here
