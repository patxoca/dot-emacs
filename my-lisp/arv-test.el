;;; arv-test.el --- utilities for testing emacs lisp

;; $Id$

;; Emacs List Archive Entry
;; Filename: arv-test.el
;; Version: $Revision$
;; Keywords:
;; Author: Alexis Roda <alexis.roda.villalonga@gmail.com>
;; Maintainer: Alexis Roda <alexis.roda.villalonga@gmail.com>
;; Created: 2013-08-25
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
;; emacs the file you want to test.
;;
;;     (require 'arv-test)

;;; Commentary:
;;


;;; History:
;;

(defvar arv-test-version-id
  "$Id$"
  "Latest modification time and version number.")

;;; Code:

(defun arv-test--mk-buffer (&optional content mode point-marker)
  "Creates a buffer with the given content, mode and point."
  (let ((buffer (generate-new-buffer "*test_buffer*")))
    (with-current-buffer buffer
      (when content
        (insert content))
      (when point-marker
        (beginning-of-buffer)
        (when (search-forward point-marker)
          (delete-char (- (length point-marker)))))
      buffer)))

(defmacro arv-test-with-buffer (content mode point-marker &rest body)
  (declare (indent 1) (debug t))
  `(save-current-buffer
     (set-buffer (arv-test--mk-buffer ,content ,mode ,point-marker))
     ,@body
     (kill-buffer)))



(provide 'arv-test)

;;; arv-test.el ends here
