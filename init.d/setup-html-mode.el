;;; html-mode.el --- configuracio de html-mode
;;; $Id$

;;; Commentary:
;;


;;; History:
;;

;;; Code:


(eval-after-load "zencoding-mode"
  '(progn
     (setq zencoding-indentation 2)
     (define-key zencoding-mode-keymap '[(control ?j)] nil)
     (set-face-attribute 'zencoding-preview-input nil
                         :box 1)
     (set-face-attribute 'zencoding-preview-output nil
                         :background "dim gray"
                         :inherit 'default
                         :box 1)))

(add-hook 'html-mode-hook
          (lambda ()
            (rainbow-mode t)
            (zencoding-mode t)))

;;; html-mode.el ends here
