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

(require 's)

;;; Code:

;; funcions utilitzades principalment als snippets

(defun arv-yas-uncamelize (text sep)
  "Retorna text sense 'camelitzar'.

Si 'text' es 'CamelCase' i 'sep' es '-' retorna 'camel-case'.

Es suposa que 'text' es un identificador valid escrit en
CamelCase. 'sep' es un string.
"
  (with-syntax-table (standard-syntax-table)
    (mapconcat 'downcase (s-split-words text) sep)))

(defun arv/yas-camelize (text)
  "Retorna TEXT 'camelitzat'.
"
  (with-syntax-table (standard-syntax-table)
    (mapconcat 'capitalize (s-split-words text) "")))

(defun arv-yas-substring (text start end)
  "Retorna un substring.

Esta versio de substring s'ajusta als limits i, a diferencia de
substring, no produeix cap error si es sobrepassen.
"
  (substring text (max 0 start) (min end (length text))))

(defun arv/string-strip-delimiters (text delimiters)
  "Strip trailing and leading chars from TEXT if they are equal
and appear in the string DELIMITERS.

\"+foo+\" \"$\" -> \"+foo+\"
\"+foo+\" \"$+\" -> \"foo\"
\"+foo-\" \"+-\" -> \"+foo-\"
"
  (if (< (length text) 2)
      text
    (let ((delimiters (delete "" (split-string delimiters "")))
          (left-char (substring text 0 1))
          (right-char (substring text (1- (length text)))))
      (if (and (string= left-char right-char)
               (member left-char delimiters))
          (substring text 1 (1- (length text)))
        text))))

(defun arv/string-replace-unwanted-chars (value predicate &optional replacement)
  "Returns a string built replacing the chars from the string
VALUE for whom the function PREDICATE returns nil with the string
REPLACEMENT.

If REPLACEMENT is omitted or nil the empty string is used as the
replacement."
  (let ((replacement (or replacement "")))
    (mapconcat (lambda (x) (cond
                       ((string= x "") "")
                       ((funcall predicate x) x)
                       (t replacement)))
               (split-string value "")
               "")))


;;                           _           _
;;   __ _ ___ ___  ___  _ __| |_ ___  __| |
;;  / _` / __/ __|/ _ \| '__| __/ _ \/ _` |
;; | (_| \__ \__ \ (_) | |  | ||  __/ (_| |
;;  \__,_|___/___/\___/|_|   \__\___|\__,_|

(defun arv/yas-today (&optional fmt)
  (let ((fmt (or fmt "%Y-%m-%d")))
    (format-time-string fmt)))

(defun arv/yas-year ()
  (format-time-string "%Y"))

(defun arv/yas-buffer-name ()
  (file-name-nondirectory (buffer-file-name)))

(defun arv/yas-buffer-name-upcase ()
  (upcase (arv/yas-buffer-name)))

(defun arv/yas-buffer-name-sans ()
  (file-name-sans-extension (arv/yas-buffer-name)))

(defun arv/yas-buffer-name-sans-upcase ()
  (upcase (arv/yas-buffer-name-sans)))

(defun arv/yas-buffer-name-sans-camelcase ()
  (arv/yas-camelize (arv/yas-buffer-name-sans)))

(defun arv/yas-author-name ()
  user-full-name)

(defun arv/yas-author-email ()
  user-mail-address)

;;    _                                _       _
;;   (_) __ ___   ____ _ ___  ___ _ __(_)_ __ | |_
;;   | |/ _` \ \ / / _` / __|/ __| '__| | '_ \| __|
;;   | | (_| |\ V / (_| \__ \ (__| |  | | |_) | |_
;;  _/ |\__,_| \_/ \__,_|___/\___|_|  |_| .__/ \__|
;; |__/                                 |_|

(defun arv/js-make-identifier (text)
  "Replace invalid chars from TEXT with an underscore in order to
make a valid javascrip identifier."
  (let ((result (arv/string-replace-unwanted-chars
                 text (lambda (x) (string-match "[$a-zA-Z0-9_]" x)) "_")))
    (if (string-match "^[$a-zA-Z_]" result)
        result
      (concat "_" result))))

(defun arv/yas-js-amd-params-from-modules (text)
  "Given the modules of and AMD `define' generates the names of
the corresponding function parametres.

'\"dojo/foo\", \"digit/a_plugin!parameters\"' -> 'foo, a_plugin'
"
  (s-join ", " (mapcar (lambda (x) (arv/js-make-identifier (car (last (s-split "/" (car (s-split "!" (arv/string-strip-delimiters (s-trim x) "'\""))))))))
                       (s-split "," text))))

(defun arv/yas-js-get-parameter-names (text)
  (mapcar (lambda (x) (s-trim x)) (delete "" (split-string text ","))))

(defun arv/yas-js-function-parameters-documentation (text &optional header)
  (let ((header (or header "Parameters"))
        (params (arv/yas-js-get-parameter-names text))
        (indent (concat "\n" (make-string (- (current-column) 2) 32) "// ")))
    (if params
        (concat indent
                header
                indent
                (mapconcat (lambda (x) (format "- %s : " x)) params indent))
      "")))

(defun arv/yas-js-function-store-parameters (text)
  (let ((params (arv/yas-js-get-parameter-names text))
        (indent (concat "\n" (make-string (current-column) 32))))
    (if params
        (concat indent
                (mapconcat (lambda (x) (format "this._%s = %s;" x x)) params indent))
      "")))

;;              _   _
;;  _ __  _   _| |_| |__   ___  _ __
;; | '_ \| | | | __| '_ \ / _ \| '_ \
;; | |_) | |_| | |_| | | | (_) | | | |
;; | .__/ \__, |\__|_| |_|\___/|_| |_|
;; |_|    |___/

(defun arv-yas-py-parse-parameters (text)
 "Parseja els arguments d'una funcií/mètode.

'foo, bar=value' -> (('foo') ('bar' 'value'))"
   (mapcar (lambda (x)
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
        (mapcar (lambda (x)
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
     (lambda (x)
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
     (lambda (x)
       (concat "self._" x " = " x))
     (mapcar
      (lambda (x)
        (s-chop-prefix "*" (s-chop-prefix "*" x)))
      (arv-yas-py-get-parameter-names text))
     indent)))


(provide 'arv-yasnippet)

;;; arv-yasnippet.el ends here
