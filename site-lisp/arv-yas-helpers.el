;;; arv-yas-helpers.el --- funcions d'utilitat en snippets -*- lexical-binding: t -*-

;; Author: Alexis Roda
;; Maintainer: Alexis Roda
;; Version: 0,1
;; Package-Requires: (s)
;; Homepage:
;; Keywords:


;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;; Aquest paquet defineix funcions que poden resultar útils al definir
;; snippets.
;;
;; Afegir a la configuració d'Emacs:
;;
;;    (require 'arv-yas-helpers)

;;; Code:

(require 's)
(require 'seq)

;;; Aquesta variable és una mica forçada però pel moment no tinc un
;;; lloc millor on ficar-la.

(defvar django-widget-types '("CheckboxInput"
                              "CheckboxSelectMultiple"
                              "DateInput"
                              "DateTimeInput"
                              "FileInput"
                              "HiddenInput"
                              "MultipleHiddenInput"
                              "NullBooleanSelect"
                              "PasswordInput"
                              "RadioSelect"
                              "Select"
                              "SelectMultiple"
                              "Textarea"
                              "TextInput"
                              "TimeInput"))

;;;                _
;;;   ___ __ _  __| | ___ _ __   ___  ___
;;;  / __/ _` |/ _` |/ _ \ '_ \ / _ \/ __|
;;; | (_| (_| | (_| |  __/ | | |  __/\__ \
;;;  \___\__,_|\__,_|\___|_| |_|\___||___/
;;;
;;; operacions amb cadenes

;;;###autoload
(defun arv/yas-uncamelize (text sep)
  "Retorna text sense 'camelitzar'.

Si 'text' es 'CamelCase' i 'sep' es '-' retorna 'camel-case'.

Es suposa que 'text' es un identificador valid escrit en
CamelCase. 'sep' es un string.
"
  (with-syntax-table (standard-syntax-table)
    (mapconcat 'downcase (s-split-words text) sep)))

;;;###autoload
(defalias 'arv-yas-uncamelize 'arv/yas-uncamelize)

;;;###autoload
(defun arv/yas-camelize (text)
  "Retorna TEXT 'camelitzat'.
"
  (with-syntax-table (standard-syntax-table)
    (mapconcat 'capitalize (s-split-words text) "")))

;;;###autoload
(defun arv/yas-substring (text start end)
  "Retorna un substring.

Esta versio de substring s'ajusta als limits i, a diferencia de
substring, no produeix cap error si es sobrepassen.
"
  (substring text (max 0 start) (min end (length text))))

;;;###autoload
(defalias 'arv-yas-substring 'arv/yas-substring)

(defun arv/yas--string-strip-delimiters (text delimiters)
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

(defun arv/yas--string-replace-unwanted-chars (value predicate &optional replacement)
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


;;;    _                                _       _
;;;   (_) __ ___   ____ _ ___  ___ _ __(_)_ __ | |_
;;;   | |/ _` \ \ / / _` / __|/ __| '__| | '_ \| __|
;;;   | | (_| |\ V / (_| \__ \ (__| |  | | |_) | |_
;;;  _/ |\__,_| \_/ \__,_|___/\___|_|  |_| .__/ \__|
;;; |__/                                 |_|
;;;
;;; javascript

;;;###autoload
(defun arv/js-make-identifier (text)
  "Replace invalid chars from TEXT with an underscore in order to
make a valid javascrip identifier."
  (let ((result (arv/yas--string-replace-unwanted-chars
                 text (lambda (x) (string-match "[$a-zA-Z0-9_]" x)) "_")))
    (if (string-match "^[$a-zA-Z_]" result)
        result
      (concat "_" result))))

;;;###autoload
(defun arv/yas-js-amd-params-from-modules (text)
  "Given the modules of and AMD `define' generates the names of
the corresponding function parametres.

'\"dojo/foo\", \"digit/a_plugin!parameters\"' -> 'foo, a_plugin'
"
  (s-join ", " (mapcar (lambda (x) (arv/js-make-identifier (car (last (s-split "/" (car (s-split "!" (arv/yas--string-strip-delimiters (s-trim x) "'\""))))))))
                       (s-split "," text))))

;;;###autoload
(defun arv/yas-js-get-parameter-names (text)
  (delete "" (mapcar 's-trim (split-string text ","))))

;;;###autoload
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

;;;###autoload
(defun arv/yas-js-function-store-parameters (text)
  (let ((params (arv/yas-js-get-parameter-names text))
        (indent (concat "\n" (make-string (current-column) 32))))
    (if params
        (concat indent
                (mapconcat (lambda (x) (format "this._%s = %s;" x x)) params indent))
      "")))


;;;              _   _
;;;  _ __  _   _| |_| |__   ___  _ __
;;; | '_ \| | | | __| '_ \ / _ \| '_ \
;;; | |_) | |_| | |_| | | | (_) | | | |
;;; | .__/ \__, |\__|_| |_|\___/|_| |_|
;;; |_|    |___/
;;;
;;; python

(defun arv/yas--ensure-two-elements (l)
  "Garanteix que la llista L té 2 elements.

Garanteix que la llista L té una longitud 2, afegint la cadena
buida per completar, si cal"
  (seq-subseq (append l '("" "")) 0 2))

;;;###autoload
(defun arv/yas-py-parse-parameters (text)
  "Parseja els arguments d'una funcií/mètode.

'foo: int, bar=value' -> (('foo' 'int' '') ('bar' '' value'))"
  (mapcar (lambda (x) (mapcar #'s-trim x))
          (mapcar (lambda (x) ;; x = "bar: str = value" -> ("bar" "str " " value")
                    (let ((parts (arv/yas--ensure-two-elements (split-string x "="))))
                      (append (arv/yas--ensure-two-elements (split-string (car parts) ":"))
                              (cdr parts))))
                  (split-string text ","))))

;;;###autoload
(defalias 'arv-yas-py-parse-parameters 'arv/yas-py-parse-parameters)


;;;###autoload
(defun arv/yas-py-get-parameter-names (text &optional exclude)
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

;;;###autoload
(defalias 'arv-yas-py-get-parameter-names 'arv/yas-py-get-parameter-names)


;;;###autoload
(defun arv/yas-py-function-parameters-documentation (text)
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

;;;###autoload
(defalias 'arv-yas-py-function-parameters-documentation 'arv/yas-py-function-parameters-documentation)


;;;###autoload
(defun arv/yas-py-constructor-store-arguments (text)
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

;;;###autoload
(defalias 'arv-yas-py-constructor-store-arguments 'arv/yas-py-constructor-store-arguments)


;;;      _
;;;  ___| |__
;;; / __| '_ \
;;; \__ \ | | |
;;; |___/_| |_|
;;;
;;; sh

;;;###autoload
(defun arv/yas-sh-getopt-case-options (text)
  "Retorna les opcions de 'case'.

Converteix:

    a:b

en:

    a) OPT_A=$OPTARG ;;
    b) OPT_B=1 ;;
"
  (let ((indent (concat "\n" (make-string (current-column) 32)))
        (result ()))
    (while (not (string= text ""))
      (let* ((char (char-to-string (aref text 0)))
             (CHAR (upcase char)))
        (if (and (> (length text) 1) (char-equal (aref text 1) ?:))
            (progn
              (setq text (substring text 2))
              (setq result (cons (format "%s) OPT_%s=$OPTARG ;;" char CHAR) result)))
          (setq result (cons (format "%s) OPT_%s=1 ;;" char CHAR) result))
          (setq text (substring text 1))
          )))
    (concat indent
            (mapconcat (lambda (x) x) (reverse result) indent))))

;;;###autoload
(defun arv/yas-sh-getopt-var-declaration (text)
  "Retorna les variables.

Converteix:

    a:b

en:

    OPT_A=
    OPT_B=
"
  (let ((result ()))
    (while (not (string= text ""))
      (let* ((char (char-to-string (aref text 0)))
             (CHAR (upcase char)))
        (if (not (string-equal char ":"))
            (setq result (cons (format "OPT_%s=" CHAR) result)))
        (setq text (substring text 1))))
    (mapconcat (lambda (x) x) (reverse result) "\n")))


;;;                  _       _
;;; __   ____ _ _ __(_) __ _| |_
;;; \ \ / / _` | '__| |/ _` | __|
;;;  \ V / (_| | |  | | (_| | |_
;;;   \_/ \__,_|_|  |_|\__,_|\__|
;;;
;;; variat

;;;###autoload
(defun arv/yas-today (&optional fmt)
  (let ((fmt (or fmt "%Y-%m-%d")))
    (format-time-string fmt)))

;;;###autoload
(defun arv/yas-year ()
  (format-time-string "%Y"))

;;;###autoload
(defun arv/yas-buffer-name ()
  (file-name-nondirectory (buffer-file-name)))

;;;###autoload
(defun arv/yas-buffer-name-upcase ()
  (upcase (arv/yas-buffer-name)))

;;;###autoload
(defun arv/yas-buffer-name-sans ()
  (file-name-sans-extension (arv/yas-buffer-name)))

;;;###autoload
(defun arv/yas-buffer-name-sans-upcase ()
  (upcase (arv/yas-buffer-name-sans)))

;;;###autoload
(defun arv/yas-buffer-name-sans-camelcase ()
  (arv/yas-camelize (arv/yas-buffer-name-sans)))

;;;###autoload
(defun arv/yas-author-name ()
  user-full-name)

;;;###autoload
(defun arv/yas-author-email ()
  user-mail-address)


(provide 'arv-yas-helpers)

;;; arv-yas-helpers.el ends here
