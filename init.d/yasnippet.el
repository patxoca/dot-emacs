;;; yasnippet.el --- configuració de yasnippet
;;; $Id$

;;; Commentary:
;;

;;; History:
;;

;;; Code:
(require 'yasnippet)
; carrega funcions d'utilitat
(require 'arv-py)

(setq yas-snippet-dirs
      (list (concat emacs-startup-dir "/shared/yasnippets/local")
            (concat emacs-startup-dir "/shared/yasnippets/snippets")))
(yas-global-mode 1)

;;; yasnippet.el ends here
