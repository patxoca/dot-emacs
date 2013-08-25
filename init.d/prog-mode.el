;;; prog-mode.el --- configuracio comuna pels modes de programació
;;; $Id$

;;; Commentary:
;;


;;; History:
;;

;;; Code:

(add-hook 'prog-mode-hook
          #'(lambda ()
              (show-paren-mode 1)
              (set-variable 'show-trailing-whitespace t)
              ))

;;; prog-mode.el ends here
