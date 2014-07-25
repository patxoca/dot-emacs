;;; ido-mode.el --- inicialitzacio de ido-mode

;; $Id$


;;; Commentary:
;;


;;; Code:

(require 'ido)

(ido-mode 1)

(when (require 'ido-vertical-mode nil 'noerror)
  (ido-vertical-mode 1))

(customize-set-variable 'ido-enable-flex-matching t)
;;; (customize-set-variable 'ido-everywhere t)
(customize-set-variable 'ido-ignore-directories
 (quote ("\\`CVS/" "\\`\\.\\./" "\\`\\./" "\\`\\.svn/" "\\`\\.ropeproject/")))

;;; 'ido-mode.el ends here
