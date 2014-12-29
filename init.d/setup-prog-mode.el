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
            ;; hi-lock
            (hi-lock-mode 1)
            (hi-lock-face-buffer "\\bFIXME:" 'hi-red-b)
            (hi-lock-face-buffer "\\bHACK:" 'hi-red-b)
            (hi-lock-face-buffer "\\bOPTIMIZE:" 'hi-red-b)
            (hi-lock-face-buffer "\\bREVIEW:" 'hi-red-b)
            (hi-lock-face-buffer "\\bTODO:" 'hi-red-b)

            (hi-lock-face-buffer "\\bIMPORTANT:" 'hi-green)
            (hi-lock-face-buffer "\\bNOTE:" 'hi-green)
            (hi-lock-face-buffer "\\bWARNING:" 'hi-green)
            ))

;;; prog-mode.el ends here
