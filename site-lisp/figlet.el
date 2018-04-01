;;; figlet.el --- FIGlet definitions for Emacs.
;;;; Copyright (c) 2007 Martin Giese. All rights reserved.
;;;;
;;;; Author: Martin Giese
;;;; Author: Dimitry Gashinsky
;;;; Id: 295eb73b-9e95-46a6-9cce-366436f9712f
;;;;

;;; Commentary:
;;

;;;; FIGlet definitions for Emacs.
;;;;
;;;; Use this to separate sections in TeX files, Program source, etc.
;;;;
;;;; customize the figlet-font-dir variable below to point to your
;;;; figlet font directory.
;;;;
;;;; M-x figlet      to get a figlet comment in standard font.
;;;; C-u M-x figlet  to be asked for the font first.
;;;; M-x banner      for an old-fashioned banner font.
;;;;
;;;; These functions use comment-region to wrap the figlet output
;;;; in comments.
;;;;

;;;  _____ ___ ____ _      _     ____  _          __  __
;;; |  ___|_ _/ ___| | ___| |_  / ___|| |_ _   _ / _|/ _|
;;; | |_   | | |  _| |/ _ \ __| \___ \| __| | | | |_| |_
;;; |  _|  | | |_| | |  __/ |_   ___) | |_| |_| |  _|  _|
;;; |_|   |___\____|_|\___|\__| |____/ \__|\__,_|_| |_|

;;; Code:

(defgroup figlet nil
  "Display large characters made up of ordinary screen characters."
  :tag "FIGlet"
  :group 'figlet
  :prefix "figlet-")

(defcustom figlet-font-dir nil
  "Change the default font directory.
FIGlet looks for fonts first in the default directory and then in
the current directory."
  :tag "FIGlet Font Directory"
  :group 'figlet
  :type '(choice (const nil) string))

(defcustom figlet-default-font nil
  "Change the default font."
  :tag "FIGlet Font Default"
  :group 'figlet
  :type '(choice (const nil) string))

(defconst figlet-font-file-regexp "\\.flf$")
(defconst figlet-match-font-name-regexp "^\\([^.]*\\)\\.flf$")

(defun figlet--font-name-for-file (filename)
  (string-match figlet-match-font-name-regexp filename)
  (match-string 1 filename))

(defun figlet--font-names ()
  (mapcar 'figlet--font-name-for-file
          (directory-files figlet-font-dir nil figlet-font-file-regexp)))

(defun figlet--read-font (prompt)
  (let* ((figlet-fonts (figlet--font-names))
         (font-alist (mapcar (lambda (x) (list x)) figlet-fonts)))
    (completing-read prompt font-alist)))

(defun figlet--call (font string)
  (push-mark)
  (apply #'call-process
         `("figlet" nil ,(current-buffer) nil
           ,@(if (null figlet-font-dir) `()
               `("-d" ,(expand-file-name figlet-font-dir)))
           ,@(if (null font) `()
               `("-f" ,font))
           ,string))
  (exchange-point-and-mark))

(defun figlet--comment-region ()
  (comment-region (region-beginning) (region-end)
                  (if (member major-mode '(emacs-lisp-mode lisp-mode scheme-mode))
                      3
                    nil)))

(defun figlet--interactive (cpa)
  (if cpa
      (let ((font (figlet--read-font "FIGlet Font: "))
            (text (read-string "FIGlet Text: ")))
        (list text font))
    (list (read-string "FIGlet Text: ") nil)))


;;;###autoload
(defun figlet (s &optional font)
  "Insert string S as a FIGlet as a comment.
Optional argument FONT font, nil for default."
  (interactive (figlet--interactive current-prefix-arg))
  (save-excursion
    (figlet--call (if (null font) figlet-default-font font) s)
    (figlet--comment-region)))

;;;###autoload
(defun figlet-no-comment (s &optional font)
  "Insert string S as a FIGlet without comment.
Optional argument FONT font, nil for default."
  (interactive  (figlet--interactive current-prefix-arg))
  (save-excursion
    (figlet--call (if (null font) figlet-default-font font) s)))

;;;###autoload
(defun banner (s)
  "Insert string S as a FIGlet comment using the banner font."
  (interactive "sFIGlet Banner Text: ")
  (figlet s "banner"))

;;;###autoload
(defun banner-no-comment (s)
  "Insert string S as a no comment FIGlet using the banner font."
  (interactive "sFIGlet Banner Text: ")
  (figlet-no-comment s "banner"))

(provide 'figlet)

;;; figlet.el ends here
