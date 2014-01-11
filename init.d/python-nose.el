;;; python-nose.el --- inicialització de nose

;; $Id$


;;; Commentary:
;;

;;; History:
;;


;;; Code:

(require 'nose-mode)

(eval-after-load "nose"
  '(nose-mode-setup-keymap))

(eval-after-load "python"
  '(add-hook 'python-mode-hook
             'nose-mode-enable-if-test-module))


;;; 'python-nose.el ends here
