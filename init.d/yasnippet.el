;;; yasnippet.el --- configuració de yasnippet
;;; $Id$

;;; Commentary:
;;

;;; History:
;;

;;; Code:

(require 'yasnippet)
(require 'arv-py)

(add-to-list 'yas-snippet-dirs
             (arv/startup-get-absolute-path "shared/yasnippets/local"))
(setq yas-prompt-functions '(yas-ido-prompt yas-x-prompt yas-completing-prompt))
(yas-global-mode 1)

;;; yasnippet.el ends here
