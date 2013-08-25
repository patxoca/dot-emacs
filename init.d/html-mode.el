;;; html-mode.el --- configuracio de html-mode
;;; $Id$

;;; Commentary:
;;


;;; History:
;;

;;; Code:

(require 'rainbow-mode)

(add-hook 'html-mode-hook
          #'(lambda ()
              (rainbow-mode t)
              ))

;;; html-mode.el ends here
