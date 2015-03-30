;;; setup-engine-mode.el --- customization for engine-mode

;; $Id$


;;; Commentary:
;;

;;; Local keybindings:
;;
;; C-M-q: does some amazing stuff

;;; History:
;;


;;; Code:

(require 'engine-mode)
(engine-mode t)

(defengine django
  "https://docs.djangoproject.com/search/?q=%s&release=11"
  "d")

(defengine dojo
  "https://www.google.es/search?q=%s+site:dojotoolkit.org"
  "D")

(defengine emacs-stack-exchange
  "http://emacs.stackexchange.com/search?q=%s"
  "e")

(defengine google
  "http://www.google.com/search?ie=utf-8&oe=utf-8&q=%s"
  "g")

(defengine github
  "https://github.com/search?ref=simplesearch&q=%s"
  "G")

(defengine rfcs
  "http://pretty-rfc.herokuapp.com/search?q=%s"
  "r")

(defengine stack-overflow
  "https://stackoverflow.com/search?q=%s"
  "s")

(defengine wikipedia
  "http://www.wikipedia.org/search-redirect.php?language=en&go=Go&search=%s"
  "w")

(defengine wordreference
  "http://www.wordreference.com/redirect/translation.aspx?w=%s&dict=enes"
  "W")

;;; engine-mode.el ends here
