;;; assorted.el --- assorted customization

;; $Id$


;;; Commentary:
;;
;; Customizations that don't fit anywhere else and are so simple to
;; deserve its own module.

;;; History:
;;


;;; Code:


(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Change window title
(when window-system
  (setq frame-title-format '("emacs %b (%f)")))


;; guide-key
(when (require 'guide-key nil t)
  (setq guide-key/guide-key-sequence '("C-x r" "C-x 4" "C-c"))
  (setq guide-key/idle-delay 0.75)
  (guide-key-mode 1))


;; popwin
(require 'popwin)
(popwin-mode 1)


;; smex
(eval-after-load "smex"
  '(progn
     (setq smex-save-file (arv/path-join user-emacs-directory "smex-items"))
     (smex-initialize)))

;;; assorted.el ends here
