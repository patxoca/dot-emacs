;;; setup-helm.el --- customization for helm

;; $Id$


;;; Commentary:
;;

;;; Local keybindings:
;;
;; C-M-q: does some amazing stuff

;;; History:
;;


;;; Code:

(require 'helm-config)

(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

(helm-mode 1)

(set-face-attribute 'helm-selection nil
                    :underline nil)


(setq helm-M-x-fuzzy-match        t
      helm-buffers-fuzzy-matching t
      helm-recentf-fuzzy-match    t
      helm-semantic-fuzzy-match   t
      helm-imenu-fuzzy-match      t
      helm-locate-fuzzy-match     t
      helm-apropos-fuzzy-match    t
      )


;;; helm.el ends here
