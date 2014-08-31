;;; workgroups.el --- inicalització de workgroups

;; $Id$


;;; Commentary:
;;


;;; History:
;;


;;; Code:

(require 'workgroups)

(setq wg-prefix-key (kbd "C-c w"))
(workgroups-mode 1)

(set-face-attribute 'wg-mode-line-face nil
                    :foreground "black"
                    :background "#2fadff")

;;; workgroups.el ends here
