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


;; visit agenda file
(defun arv/org--get-agenda-files ()
  (if (listp org-agenda-files)
      org-agenda-files
    (let ((tmp nil)
          (line nil))
      (with-temp-buffer
        (insert-file-contents org-agenda-files)
        (goto-char (point-min))
        (while (= 0 (progn
                      (setq line (s-trim-right (thing-at-point 'line)))
                      (when line
                        (add-to-list 'tmp line))
                      (forward-line)))))
      tmp)))

;;;###autoload
(defun arv/org-visit-agenda-file ()
  ""
  (interactive)
  (find-file
   (ido-completing-read "Agenda file:" (arv/org--get-agenda-files) nil t)))


(defun arv/org-emphasize (char)
  (if mark-active
      (org-emphasize char)
    (insert char)))


(defun arv/--member-nested-one-level (e ll)
  (while (and ll (not (member e (car ll))))
    (setq ll (cdr ll)))
  ll)

;;;###autoload
(defun arv/org-remove-reduntant-tags ()
  "Walks the entire buffer removing redundant tags."
  (interactive)
  (when (eq major-mode 'org-mode)
    (let ((seen-so-far nil))
      (save-excursion
        (org-map-entries
         (lambda ()
           (let ((alltags (split-string (or (org-entry-get (point) "ALLTAGS") "") ":"))
                 (hdlevel (nth 1 (org-heading-components)))
                 local)
             (while (<= hdlevel (length seen-so-far))
               (setq seen-so-far (cdr seen-so-far)))
             (dolist (tag alltags)
               (if (arv/--member-nested-one-level tag seen-so-far)
                   (org-toggle-tag tag 'off)
                 (setq local (cons tag local))))
             (setq seen-so-far (cons local seen-so-far))))
         t nil)))))


(provide 'arv-org)

;;; arv-org.el ends here
