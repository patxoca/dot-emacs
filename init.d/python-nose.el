;;; python-nose.el --- inicialització de nose

;; $Id$


;;; Commentary:
;;

;;; History:
;;


;;; Code:


(eval-after-load "python"
  '(progn
     (require 'nose-mode)
     (nose-mode-setup-keymap)
     (add-hook 'python-mode-hook
               'nose-mode-enable-if-test-module)))


;;; 'python-nose.el ends here
