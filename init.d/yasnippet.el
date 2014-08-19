;;; yasnippet.el --- configuració de yasnippet
;;; $Id$

;;; Commentary:
;;

;;; History:
;;

;;; Code:

(require 'yasnippet)
(require 'arv-py)
(require 'arv-yasnippet)

(add-to-list 'yas-snippet-dirs
             (arv/startup-get-absolute-path "shared/yasnippets/local"))
(setq yas-prompt-functions '(yas-ido-prompt yas-x-prompt yas-completing-prompt))
(yas-global-mode 1)

(add-to-list 'auto-mode-alist '("\\.yas$" . snippet-mode))

;;; yasnippet.el ends here
