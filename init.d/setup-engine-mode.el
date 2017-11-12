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

(defengine diec2
  "dlc.iec.cat/results.asp?txtEntrada=%s"
  :keybinding "c"
  :term-transformation-hook (lambda (term) (encode-coding-string term latin-1)))

(defengine django
  "https://docs.djangoproject.com/search/?q=%s&release=11"
  :keybinding "d")

(defengine dojo
  "https://www.google.es/search?q=%s+site:dojotoolkit.org"
  :keybinding "D")

(defengine emacs-stack-exchange
  "http://emacs.stackexchange.com/search?q=%s"
  :keybinding "e")

(defengine google
  "http://www.google.com/search?ie=utf-8&oe=utf-8&q=%s"
  :keybinding "g")

(defengine github
  "https://github.com/search?ref=simplesearch&q=%s"
  :keybinding "G")

(defengine rfcs
  "http://pretty-rfc.herokuapp.com/search?q=%s"
  :keybinding "r")

(defengine stack-overflow
  "https://stackoverflow.com/search?q=%s"
  :keybinding "s")

(defengine wikipedia
  "http://www.wikipedia.org/search-redirect.php?language=en&go=Go&search=%s"
  :keybinding "w")

(defengine wordreference
  "http://www.wordreference.com/redirect/translation.aspx?w=%s&dict=enes"
  :keybinding "W")

(defengine z80heaven
  "http://z80-heaven.wikidot.com/instructions-set:%s"
  :keybinding "z")
;;; engine-mode.el ends here
