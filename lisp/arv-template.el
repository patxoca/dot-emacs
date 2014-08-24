;;; arv-template.el --- insert skeletons into new files

;; $Id$

;; Emacs List Archive Entry
;; Filename: arv-template.el
;; Version: $Revision$
;; Keywords:
;; Author: Alexis Roda <alexis.roda.villalonga@gmail.com>
;; Maintainer: Alexis Roda <alexis.roda.villalonga@gmail.com>
;; Created: 2014-08-06
;; Description: Auto insertion of skeletons for new files built on top
;; of yasnippet.
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
;;     (arv/template-setup)
;;
;; Create snippets keyed 'header'. If you prefer other key customize
;; the variable `arv/template-snippet-key'.

;;; Commentary:
;;

;;; Implementation notes:
;;
;; This works by letting `yasnippet' expand the value of the
;; customizable variable `arv/template-snippet-key' ("header" by
;; default) whenever visiting a new file. Two questions arise:
;;
;; * when to insert/expand the key?
;;
;; * what to do if the expansion fails?
;;
;; In order to find the right snippet the buffer must be already in
;; the right major mode. My first attempt was hooking a function to
;; `find-file-not-found-functions', but this didn't work because at
;; that point the buffer is still in `fundamental-mode'.
;;
;; So I have decided to track the mode changes using
;; `after-change-major-mode-hook', this has a couple of drawbacks:
;;
;; * apparently a newly created buffer is put in `fundamental-mode'
;;   and then switched to its final mode, so the functions hooked to
;;   `after-change-major-mode-hook' should account for being called
;;   (just?) twice.
;;
;; * the function must determine by itself if the buffer is visiting a
;;   new file or an existing one.
;;
;; Finally, if the expansion fails the key ("header") must be
;; removed. At first I tried with `undo' but emacs complained "no
;; further undo information" so I decided to explicitly delete the
;; key.

;;; History:
;;

(defvar arv/template-version-id
  "$Id$"
  "Latest modification time and version number.")

;;; Code:

(defgroup arv/template nil
  "Insert documentation here."
  :group 'arv)

(defcustom arv/template-snippet-key "header"
  "Name of the snippet."
  :group 'arv/template
  :type  'string
  :safe  'stringp)


(defun arv/template-insert-header ()
  (when (and (not buffer-read-only)
             (not (eq major-mode 'fundamental-mode))
             (buffer-file-name)
             (save-excursion (goto-char (point-min))
                             (eobp)) ; empty buffer?
             (not (file-exists-p (buffer-file-name))))
    (let ((pos (point)))
      (insert arv/template-snippet-key)
      (unless (yas-expand-from-trigger-key)
        (goto-char pos)
        (delete-char (length arv/template-snippet-key) nil)
        (set-buffer-modified-p nil)))))


;;;###autoload
(defun arv/template-setup ()
  (add-hook 'after-change-major-mode-hook 'arv/template-insert-header))


(provide 'arv-template)

;;; arv-template.el ends here
