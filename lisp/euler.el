;;; euler.el --- helper for project euler

;; $Id$

;; Emacs List Archive Entry
;; Filename: euler.el
;; Version: $Revision$
;; Keywords:
;; Author: Alexis Roda <alexis.roda.villalonga@gmail.com>
;; Maintainer: Alexis Roda <alexis.roda.villalonga@gmail.com>
;; Created: 2014-12-25
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
;;     (require 'euler)
;;
;; or use autoload:
;;
;;      (autoload 'euler-mode "euler" "" t)

;;; Commentary:
;;


;;; History:
;;

(require 's)

(defvar euler-version-id
  "$Id$"
  "Latest modification time and version number.")

(defun euler--callback (status buffer problem-no)
  (let (text title)
    (search-forward-regexp "^ +projecteuler.net")
    (right-char 2)
    (setq title (buffer-substring-no-properties (point) (point-at-eol)))
    (delete-region (point-min) (point))
    (insert (format "Problem %i: " problem-no))
    (end-of-line)
    (right-char 2)
    (kill-line 4)
    (setq text (buffer-substring-no-properties (point-min) (point-max)))
    (with-current-buffer buffer
      (set-visited-file-name (format "%04i-%s.py" problem-no (s-replace " " "_" title)))
      (python-mode)
      (insert text)
      (comment-region (point-min) (point-max)))
    (switch-to-buffer buffer)
    (goto-char (point-max))))

;;;###autoload
(defun euler-get-problem (problem-no)
  (interactive "P")
  (let ((b (generate-new-buffer (format "%03i.py" problem-no)))
        (url (format "http://www.w3.org/services/html2txt?url=https://projecteuler.net/problem=%i&noinlinerefs=on&nonums=on" problem-no)))
    (url-retrieve url 'euler--callback (list b problem-no))
    ))

(provide 'euler)

;;; euler.el ends here
