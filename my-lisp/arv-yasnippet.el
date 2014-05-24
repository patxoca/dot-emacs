;;; arv-yasnippet.el --- yasnippet utilities

;; $Id$

;; Emacs List Archive Entry
;; Filename: arv-yasnippet.el
;; Version: $Revision$
;; Keywords:
;; Author: Alexis Roda <alexis.roda.villalonga@gmail.com>
;; Maintainer: Alexis Roda <alexis.roda.villalonga@gmail.com>
;; Created: 2014-05-23
;; Description:
;; URL:
;; Compatibility: Emacs24

;; COPYRIGHT NOTICE
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more
;; details. http://www.gnu.org/copyleft/gpl.html

;;; Install:

;; Put this file on your Emacs-Lisp load path and add following into
;; emacs startup file.
;;
;;     (require 'arv-yasnippet)
;;
;; or use autoload:
;;
;;      (autoload 'arv-yasnippet-mode "arv-yasnippet" "" t)

;;; Commentary:
;;
;; Funcions utilitzades en snippets.

;;; History:
;;

(defvar arv-yasnippet-version-id
  "$Id$"
  "Latest modification time and version number.")

;;; Code:

;; funcions utilitzades principalment als snippets

(defun arv-uncamelize (text sep)
  "Retorna text sense 'camelitzar'.

Si 'text' es 'CamelCase' i 'sep' es '-' retorna 'camel-case'.

Es suposa que 'text' es un identificador valid escrit en
CamelCase. 'sep' es un string.
"
  (mapconcat 'downcase (s-split-words text) sep))

(defun arv-substring (text start end)
  "Retorna un substring.

Esta versio de substring s'ajusta als limits i, a diferencia de
substring, no produeix cap error si es sobrepassen.
"
  (substring text (max 0 start) (min end (length text))))

(defun arv-chop (c s)
  "s-chop-prefix + s-chop-suffix"
  (s-chop-suffix c (s-chop-prefix c s)))

(defun arv-yas-dojo-params-from-modules (text)
  ""
  (s-join ", " (mapcar (lambda (x)
                         (car (last (s-split "/" (arv-chop "\"" (arv-chop " " x))))))
                       (s-split "," text))))

(provide 'arv-yasnippet)

;;; arv-yasnippet.el ends here
