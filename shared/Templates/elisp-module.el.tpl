;;; (>>>FILE<<<) --- (>>>POINT<<<)

;; $Id:$

;; Emacs List Archive Entry
;; Filename: (>>>FILE<<<)
;; Version: $Revision:$
;; Keywords:
;; Author: Alexis Roda <alexis.roda.villalonga@gmail.com>
;; Maintainer: Alexis Roda <alexis.roda.villalonga@gmail.com>
;; Created: (>>>ISO_DATE<<<)
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
;;     (require '(>>>FILE_SANS<<<))
;;
;; or use autoload:
;;
;;      (autoload '(>>>FILE_SANS<<<)-mode "(>>>FILE_SANS<<<)" "" t)

;;; Commentary:
;;


;;; History:
;;

(defvar (>>>FILE_SANS<<<)-version-id
  "$Id:$"
  "Latest modification time and version number.")

;;; Code:

(defgroup (>>>FILE_SANS<<<) nil
  "Insert documentation here.")

(defcustom (>>>FILE_SANS<<<)-option nil
  "Insert documentation here."
  :group '(>>>FILE_SANS<<<)
  :type  'string
  :safe  'stringp)


(provide '(>>>FILE_SANS<<<))

;;; (>>>FILE<<<) ends here
