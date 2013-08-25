;;; html-mode.el --- configuracio de html-mode
;;; $Id$

;;; Commentary:
;;


;;; History:
;;

;;; Code:

(require 'rainbow-mode)
(require 'zencoding-mode)


(eval-after-load "zencoding-mode"
  '(progn
     (define-key zencoding-mode-keymap '[(control ?j)] nil)))

(add-hook 'html-mode-hook
          #'(lambda ()
              (rainbow-mode t)
              (zencoding-mode t)))

;;; html-mode.el ends here
