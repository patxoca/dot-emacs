;;; arv-py-ffap.el --- 

;; $Id$

;; Emacs List Archive Entry
;; Filename: arv-py-ffap.el
;; Version: $Revision$
;; Keywords:
;; Author: Alexis Roda <alexis.roda.villalonga@gmail.com>
;; Maintainer: Alexis Roda <alexis.roda.villalonga@gmail.com>
;; Created: 2013-12-30
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
;;     (require 'arv-py-ffap)
;;
;; or use autoload:
;;
;;      (autoload 'arv-py-ffap-mode "arv-py-ffap" "" t)

;;; Commentary:
;;
;; Carrega un mòdul python a partir del import en que es troba el
;; cursor:
;;
;; * Per obtindre els paràmetres del import utilitza expressions
;;   regulars. No es cotemplent tots els casos.

;; * Per esbrinar la ruta del mòdul es delega en un script python
;;   passat a un interpret inferior.

;;; History:
;;

(defvar arv-py-ffap-version-id
  "$Id$"
  "Latest modification time and version number.")

;;; Code:

(defconst arv-py--ffap-dotted-id-regexp "\\([[:alnum:]_]+\\(?:\\.[[:alnum:]_]+\\)*\\)")
(defconst arv-py--ffap-id-regexp "\\([[:alnum:]_]+\\)")
(defvar arv-py--ffap-import-regexps-alist
  (list
   (cons
    (format "from %s import %s" arv-py--ffap-dotted-id-regexp arv-py--ffap-id-regexp)
    (lambda () (list (match-string-no-properties 1) (match-string-no-properties 2))))
   (cons
    (format "import %s" arv-py--ffap-dotted-id-regexp)
    (lambda () (list (match-string-no-properties 1) nil)))))

(defun arv-py--ffap-get-import-at-point ()
  "DOCSTRING"
  (interactive)
  (let ((foo (-first (lambda (i)
                       (save-excursion
                         (beginning-of-line)
                         (re-search-forward (car i) (point-at-eol) t)))
                     arv-py--ffap-import-regexps-alist)))
    (if (null foo)
        nil
      (funcall (cdr foo)))))

(defun arv-py--ffap-generate-code (name fromlist)
""
(format "
try:
    import %s
except ImportError:
    print 'ERROR'
else:
    module = %s
    objname = '%s'
    if objname:
        obj = getattr(module, objname, None)
        if hasattr(obj, '__file__'):
            module = obj
    path = module.__file__
    if path.endswith('pyc'):
        path = path[:-1]
    print path
" name name (or fromlist "")))

(defun arv-py-ffap ()
  ""
  (interactive)
  (let ((foo (arv-py--ffap-get-import-at-point)))
    (if (null foo)
        (message "Cannot parse import")
      (let ((path (python-shell-send-string-no-output
                   (arv-py--ffap-generate-code (car foo) (cadr foo)))))
        (message (format "**** %s\n" path))
        (if (string= path "ERROR")
            (message "Import error")
          (find-file path))))))

(provide 'arv-py-ffap)

;;; arv-py-ffap.el ends here
