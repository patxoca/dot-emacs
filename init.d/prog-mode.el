;;; prog-mode.el --- configuracio comuna pels modes de programació
;;; $Id$

;;; Commentary:
;;


;;; History:
;;

;;; Code:

(eval-after-load "linum"
  '(progn
     (require 'relative-linum)))


(add-hook 'prog-mode-hook
          (lambda ()
            (linum-mode 0)
            (show-paren-mode 1)
            (set-variable 'show-trailing-whitespace t)
            ))

;;; prog-mode.el ends here
