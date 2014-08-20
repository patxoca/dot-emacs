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

(defun arv/org-ecfg--link-at-point-get-range ()
  "Returns a list with the start and end position for the link at
point. A link is delimited by blanks and beggining/end of line."
  (let ((start (save-excursion
                 (skip-syntax-backward "^-" (line-beginning-position))
                 (point)))
        (end (save-excursion
               (skip-syntax-forward "^-" (line-end-position))
               (point))))
    (list start end)))

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

(defvar arv/org-ecfg-insert-link-at-point-history nil)

;;;###autoload
(defun arv/org-ecfg-insert-link-at-point (arg)
  "Insert a ecfg link using the text around the point. By default
the description part is the same as the text. Use the prefix
command in order to edit the description."
  (interactive "*P")
  (let* ((range (if mark-active
                    (list (point) (mark))
                  (arv/org-ecfg--link-at-point-get-range)))
         (text (apply 'buffer-substring-no-properties range))
         (link (if (s-starts-with-p "ecfg:" text)
                   text
                 (concat "ecfg:" text)))
         (description (if (s-starts-with-p "ecfg:" text)
                          (mapconcat 'identity (cdr (s-split ":" text)) ":")
                        text)))
    (if mark-active
        (deactivate-mark))
    (if arg
        (setq description (read-from-minibuffer "Description:" description
                                                nil nil
                                                'arv/org-ecfg-insert-link-at-point-history)))
    (apply 'delete-region range)
    (insert (format "[[%s][%s]]" link description))))



(provide 'arv-org)

;;; arv-org.el ends here
