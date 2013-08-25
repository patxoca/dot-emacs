;;; css-mode.el --- configuracio de css-mode
;;; $Id$

;;; Commentary:
;;


;;; History:
;;

;;; Code:

(require 'css-mode)
(require 'rainbow-mode)

(add-hook 'css-mode-hook
          #'(lambda ()
              (rainbow-mode t)
              ))

;;; css-mode.el ends here
