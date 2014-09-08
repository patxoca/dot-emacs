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


;; emacs faces
(set-face-attribute 'compilation-error nil
                    :underline nil
                    :weight 'normal)
(set-face-attribute 'cursor  nil
                    :background "white")
(set-face-attribute 'default nil
                    :height 113)


;; highlight-indentation
(eval-after-load "highlight-identation"
  '(progn
     (set-face-attribute 'highlight-indent-face nil
                         :background "#303739")))


;; popwin
(require 'popwin)
(popwin-mode 1)


;; Non-nil means a single space does not end a sentence. This is
;; relevant for filling.
(setq sentence-end-double-space nil)


;; smex
(eval-after-load "smex"
  '(progn
     (setq smex-save-file (arv/path-join user-emacs-directory "smex-items"))
     (smex-initialize)))


;; face-remap
(setq text-scale-mode-step 1.1)


;; visible-bell flashes the frame instead of ringing a bell
(setq visible-bell t)


;;; assorted.el ends here
