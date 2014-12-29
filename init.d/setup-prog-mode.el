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
            (hi-lock-face-buffer "\\<FIXME" 'hi-red-b)
            (hi-lock-face-buffer "\\<HACK" 'hi-red-b)
            (hi-lock-face-buffer "\\<OPTIMIZE" 'hi-red-b)
            (hi-lock-face-buffer "\\<REVIEW" 'hi-red-b)
            (hi-lock-face-buffer "\\<TODO" 'hi-red-b)

            (hi-lock-face-buffer "\\<IMPORTANT" 'hi-green)
            (hi-lock-face-buffer "\\<NOTE" 'hi-green)
            (hi-lock-face-buffer "\\<WARNING" 'hi-green)
            ))

;;; prog-mode.el ends here
