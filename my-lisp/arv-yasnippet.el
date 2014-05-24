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

(defun arv-yas-uncamelize (text sep)
  "Retorna text sense 'camelitzar'.

Si 'text' es 'CamelCase' i 'sep' es '-' retorna 'camel-case'.

Es suposa que 'text' es un identificador valid escrit en
CamelCase. 'sep' es un string.
"
  (mapconcat 'downcase (s-split-words text) sep))

(defun arv-yas-substring (text start end)
  "Retorna un substring.

Esta versio de substring s'ajusta als limits i, a diferencia de
substring, no produeix cap error si es sobrepassen.
"
  (substring text (max 0 start) (min end (length text))))

(defun arv-chop (c s)
  "s-chop-prefix + s-chop-suffix"
  (s-chop-suffix c (s-chop-prefix c s)))

;;    _                                _       _
;;   (_) __ ___   ____ _ ___  ___ _ __(_)_ __ | |_
;;   | |/ _` \ \ / / _` / __|/ __| '__| | '_ \| __|
;;   | | (_| |\ V / (_| \__ \ (__| |  | | |_) | |_
;;  _/ |\__,_| \_/ \__,_|___/\___|_|  |_| .__/ \__|
;; |__/                                 |_|

(defun arv-yas-dojo-params-from-modules (text)
  ""
  (s-join ", " (mapcar (lambda (x)
                         (car (last (s-split "/" (arv-chop "\"" (arv-chop " " x))))))
                       (s-split "," text))))


;;              _   _
;;  _ __  _   _| |_| |__   ___  _ __
;; | '_ \| | | | __| '_ \ / _ \| '_ \
;; | |_) | |_| | |_| | | | (_) | | | |
;; | .__/ \__, |\__|_| |_|\___/|_| |_|
;; |_|    |___/

(defun arv-yas-py-parse-parameters (text)
 "Parseja els arguments d'una funcií/mètode.

'foo, bar=value' -> (('foo') ('bar' 'value'))"
   (mapcar '(lambda (x)
              (mapcar 's-trim (split-string x "=")))
           (split-string text ",")))


(defun arv-yas-py-get-parameter-names (text &optional exclude)
  "Retorna el nom dels paràmetres.

Mira de netejar anomalies com parametres sense nom. Si
s'especifica un valor diferent de `nil' per `exclude' s'exclouen
el paràmetres '*args' i '**kw' si estan presents (es comprova que
comencin per * no el nom concret).

'foo, bar=value' -> ('foo', 'bar')
'foo,, bar=value' -> ('foo', 'bar')
'foo, bar=value, *args' -> ('foo', 'bar', '*args')

Especificant `exclude':

'foo, bar=value, *args' -> ('foo', 'bar')"
  (delq nil
        (mapcar '(lambda (x)
                   (let ((name (nth 0 x)))
                     (unless (or (string= name "")
                                 (and (not (null exclude))
                                      (s-starts-with-p "*" name)))
                       name)))
                (arv-yas-py-parse-parameters text))))


(defun arv-yas-py-function-parameters-documentation (text)
  "Retorna la documentació pels paràmetres d'una funció.

Converteix:

  foo, bar=1234, *args, **kw

en:

  :param foo:
  :param bar:
"
  (let* ((indent (concat "\n" (make-string (current-column) 32))))
    (mapconcat
     '(lambda (x)
        (concat ":param " x ":"))
     (arv-yas-py-get-parameter-names text 't)
     indent)))


(defun arv-yas-py-constructor-store-arguments (text)
  "Retorna l'assignació a atributs en el constructor.

Converteix:

  foo, bar=1234, *args, **kw

en:

  self._foo = foo
  self._bar = bar
  self._args = args
  self._kw = kw
"
  (let* ((indent (concat "\n" (make-string (current-column) 32))))
    (mapconcat
     '(lambda (x)
        (concat "self._" x " = " x))
     (mapcar
      '(lambda (x)
         (s-chop-prefix "*" (s-chop-prefix "*" x)))
      (arv-yas-py-get-parameter-names text))
     indent)))


(provide 'arv-yasnippet)

;;; arv-yasnippet.el ends here
