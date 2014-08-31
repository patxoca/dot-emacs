;;; erc.el --- personalitzacio del erc
;;; (c) 2006 Alexis Roda
;;; $Id$

;;; Commentary:
;;;
;;; arxiu de configuracio de erc

;;; History:
;;;
;;; consultar log de svn

;;; Code:

(require 'erc)
(require 'erc-stamp)
(require 'easymenu)

(setq browse-url-browser-function 'w3m)

(defun chatear()
  "Chatejar al IRC amb ERC."
  (interactive)
  (setq erc-autojoin-channels-alist '(("freenode.net" "#emacs-es"
                                                      "#python-es")))
  (erc-timestamp-mode 1)
  (erc-select :server "irc.freenode.net"
              :port 6667
              :nick "arroda"
              :password (getenv "NICKSERVPASS")
              :full-name "Alexis Roda")

  (setq erc-track-exclude-types '("JOIN"
                                  "PART"
                                  "QUIT"
                                  "NICK"
                                  "MODE"
                                  "Leaving"
                                  )))

; afegeix una opcio IRC al menu TOOL per iniciar erc
(easy-menu-add-item nil '("tools")
                    ["IRC" chatear t])

;; estic acostumbrat a C-RET pel gaim i de vegades se m'escapa
(define-key erc-mode-map '[(control return)] "\C-m")

(add-hook 'erc-mode-hook
          (lambda ()
            (hl-line-mode nil)
            (set-variable 'show-trailing-whitespace nil)))

;;; erc.el ends here
