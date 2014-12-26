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

;;; Code:

;; (defgroup euler nil
;;   "Insert documentation here.")

;; (defcustom euler-option nil
;;   "Insert documentation here."
;;   :group 'euler
;;   :type  'string
;;   :safe  'stringp)


(defun euler--get-title (buffer)
  (save-excursion
    (goto-char (point-min))
    (if (search-forward-regexp "<h2>\\([^<]*\\)</h2>" (point-max) t)
        (match-string-no-properties 1)
      "** no title **")))

(defun euler--get-description (buffer)
  (save-excursion
    (goto-char (point-min))
    (if (search-forward "<div class=\"problem_content\" role=\"problem\">" (point-max) t)
        (let ((start (point))
              (end (search-forward-regexp "^</div>" (point-max) t))
              body)
          (s-replace-all '(("" . "")
                           ("<p>" . "")
                           ("</p>" . "\n"))
                         (buffer-substring-no-properties start (- end (length "</div>")))))
        "** no description **")))

(defun euler--remove-markup ()
  (save-excursion
    (goto-char (point-min))
    (replace-regexp "<[^>]*>" "")))

(defun euler--callback (status buffer problem-no)
  (let ((title (euler--get-title (current-buffer)))
        (description (euler--get-description (current-buffer))))
    (with-current-buffer buffer
      (set-visited-file-name (format "%04i-%s.py" problem-no (s-replace " " "_" title)))
      (insert (format "Problem %s: %s\n" problem-no title))
      (save-excursion
        (insert description))
      (euler--remove-markup)
      (python-mode)
      (fill-region (point-min) (point-max))
      (comment-region (point-min) (point-max)))
    (switch-to-buffer buffer)
    (goto-char (point-max))))

;;;###autoload
(defun euler-get-problem (problem-no)
  (interactive "P")
  (let ((b (generate-new-buffer (format "%03i.py" problem-no)))
        (url (format "https://projecteuler.net/problem=%i" problem-no)))
    (url-retrieve url 'euler--callback (list b problem-no))
    ))

(provide 'euler)

;;; euler.el ends here
