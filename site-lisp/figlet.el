;;; figlet.el --- FIGlet definitions for Emacs.

;;;; Copyright (c) 2007 Martin Giese. All rights reserved.
;;;; Copyright (c) 2018 Alexis Roda. All rights reserved.
;;;;
;;;; Author: Martin Giese
;;;; Author: Dimitry Gashinsky
;;;; Author: Alexis Roda
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

(defconst figlet--font-file-regexp "\\.flf$")
(defconst figlet--match-font-name-regexp "^\\([^.]*\\)\\.flf$")

(defun figlet--font-name-for-file (filename)
  (string-match figlet--match-font-name-regexp filename)
  (match-string 1 filename))

(defun figlet--font-names ()
  (mapcar 'figlet--font-name-for-file
          (directory-files figlet-font-dir nil figlet--font-file-regexp)))

(defun figlet--read-font (prompt)
  (let* ((figlet-fonts (figlet--font-names))
         (font-alist (mapcar (lambda (x) (list x)) figlet-fonts)))
    (completing-read prompt font-alist)))

(defun figlet--ensure-bolp ()
    (unless (bolp)
      (end-of-line)
      (newline)))

(defun figlet--call (font string &optional post-process)
  (figlet--ensure-bolp)
  (ignore
   (let ((begin (point)))
     (apply #'call-process
            `("figlet" nil ,(current-buffer) nil
              ,@(when figlet-font-dir
                  `("-d" ,(expand-file-name figlet-font-dir)))
              ,@(when font
                  `("-f" ,font))
              ,string))
     (figlet--ensure-bolp)
     (delete-trailing-whitespace begin (point))
     (when post-process
       (funcall post-process begin (point))))))

(defun figlet--comment-region (begin end)
  (comment-region begin end
                  (if (member major-mode '(emacs-lisp-mode lisp-mode scheme-mode))
                      3
                    nil)))

(defun figlet--interactive (cpa)
  (if (and cpa figlet-font-dir)
      (let ((font (figlet--read-font "FIGlet Font: "))
            (text (read-string "FIGlet Text: ")))
        (list text font))
    (list (read-string "FIGlet Text: ") nil)))


;;;###autoload
(defun figlet (s &optional font)
  "Insert string S as a FIGlet as a comment.
Optional argument FONT font, nil for default."
  (interactive (figlet--interactive current-prefix-arg))
  (figlet--call (or font figlet-default-font) s #'figlet--comment-region))

;;;###autoload
(defun figlet-no-comment (s &optional font)
  "Insert string S as a FIGlet without comment.
Optional argument FONT font, nil for default."
  (interactive  (figlet--interactive current-prefix-arg))
  (figlet--call (or font figlet-default-font) s))

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

;;;###autoload
(defun figlet-display-fonts ()
  "Display the font gallery in a buffer."
  (interactive)
  (let ((buffer (get-buffer-create "*figlet-fonts*")))
    (with-current-buffer buffer
      (setq buffer-read-only nil)
      (erase-buffer)
      (dolist (font (figlet--font-names) nil)
        (insert "Font: " font)
        (newline 2)
        (figlet-no-comment "Lazy dog." font)
        (newline 2))
      (set-buffer-modified-p nil)
      (setq buffer-read-only t)
      (goto-char (point-min)))
    (set-window-buffer nil buffer)))

(provide 'figlet)

;;; figlet.el ends here
