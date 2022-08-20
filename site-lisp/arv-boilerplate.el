;;; arv-boilerplate.el --- insert boilerplate on file creation -*- lexical-binding: t -*-

;; Author: Alexis Roda
;; Maintainer: Alexis Roda
;; Version: 0.2
;; Package-Requires: (yasnippet)
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

;; Insert boilerplate when creating a new file. Based on yasnippet
;; snippets.
;;
;; This works by inserting the text "header" (customizable changing
;; the value of the variable arv/boilerplate-snippet-key) and letting
;; yasnippet expand the snippet. This is done only on empty buffers
;; that are visiting a new file and are not read only.
;;
;; In order fot this to work you should provide snippets for the
;; "header" key.
;;
;; Add to your emacs config:
;;
;;   (require 'arv/boilerplate)
;;   (add-to-list 'yas-snippet-dirs "/path/to/headers/directory)
;;   (arv/boilerplate-setup)
;;
;; Two questions arise in order to implement this:
;;
;; - when to insert/expand the key?
;;
;; - what to do if the expansion fails?
;;
;; In order to find the right snippet the buffer must be already in
;; the right major mode. My first attempt was hooking a function to
;; find-file-not-found-functions, but this didn't work because at that
;; point the buffer is still in fundamental-mode.
;;
;; So I have decided to track the mode changes using
;; after-change-major-mode-hook, this has a couple of drawbacks:
;;
;; - apparently a newly created buffer is put in fundamental-mode and
;;   then switched to its final mode, so the functions hooked to
;;   after-change-major-mode-hook should account for being called at
;;   least twice.
;;
;; - the function must determine by itself if the buffer is visiting a
;;   new file or an existing one.
;;
;; Finally, if the expansion fails the key ("header") must be removed.
;; At first I tried with undo but emacs complained "no further undo
;; information" so I decided to explicitly delete the key.

;;; Code:

(require 'yasnippet)

(defgroup arv/boilerplate nil
  "Customization group for arv/boilerplate."
  :group 'arv)

(defcustom arv/boilerplate-snippet-key "header"
  "Name of the snippet used to expand de boilerplate."
  :group 'arv/boilerplate
  :type  'string
  :safe  'stringp)


(defun arv/boilerplate-insert-header ()
  (when (and (not buffer-read-only)
             (zerop (buffer-size))
             (not (eq major-mode 'fundamental-mode))
             (buffer-file-name)
             (not (file-exists-p (buffer-file-name))))
    (let ((pos (point)))
      (insert arv/boilerplate-snippet-key)
      (unless (yas-expand-from-trigger-key)
        (goto-char pos)
        (delete-char (length arv/boilerplate-snippet-key) nil)
        (set-buffer-modified-p nil)))))


;;;###autoload
(defun arv/boilerplate-setup ()
  (add-hook 'after-change-major-mode-hook #'arv/boilerplate-insert-header))

(provide 'arv-boilerplate)

;;; arv-boilerplate.el ends here
