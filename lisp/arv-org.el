;;; arv-org.el --- additional functionality for org-mode

;; $Id$

;; Emacs List Archive Entry
;; Filename: arv-org.el
;; Version: $Revision$
;; Keywords:
;; Author: Alexis Roda <alexis.roda.villalonga@gmail.com>
;; Maintainer: Alexis Roda <alexis.roda.villalonga@gmail.com>
;; Created: 2014-08-20
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

;;; Commentary:
;;

;;; History:
;;

(require 's)

(defvar arv-org-version-id
  "$Id$"
  "Latest modification time and version number.")


;;; 'ecfg:' links
;;
;; 'ecfg:' URLs point to files in my emacs configuration. The links
;; are relative to `emacs-startup-dir'. Useful for tasks/notes when
;; hacking my emacs configuration.

(defun arv/org-ecfg--relativize-to-startup-dir-maybe (path)
  "Converts .PATH into a path relative to
`emacs-startup-dir'. Returns the relative path or nil if PATH is
outside `emacs-startup-dir'.

PATH must be absolute."
  (let ((abs-startup-dir (file-name-as-directory (expand-file-name emacs-startup-dir))))
    (if (s-starts-with-p abs-startup-dir path)
        (s-chop-prefix abs-startup-dir path)
      nil)))

;;;###autoload
(defun arv/org-ecfg-open (path)
  "Visit the file within `emacs-startup-dir'."
  (find-file (arv/path-join emacs-startup-dir path)))

;;;###autoload
(defun arv/org-ecfg-store-link ()
  "Store a link to an emacs config file."
  (let ((link (arv/org-ecfg--relativize-to-startup-dir-maybe (buffer-file-name))))
    (when link
      (org-store-link-props
       :type "ecfg"
       :link (concat "ecfg:" link)
       :description (format "%s" link)))))



(provide 'arv-org)

;;; arv-org.el ends here
