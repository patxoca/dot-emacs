;;; ido-mode.el --- inicialitzacio de ido-mode

;; $Id$


;;; Commentary:
;;


;;; Code:

(defun arv/ido-vertical-define-keys ()
  ;; more intuitive keybinds for vertical-mode
  (define-key ido-completion-map (kbd "<down>") 'ido-next-match)
  (define-key ido-completion-map (kbd "<up>") 'ido-prev-match)
  (define-key ido-completion-map (kbd "<left>") 'ido-prev-match-dir)
  (define-key ido-completion-map (kbd "<right>") 'ido-next-match-dir))

(ido-mode 1)

(when (require 'ido-vertical-mode nil 'noerror)
  (ido-vertical-mode 1)
  (add-hook 'ido-setup-hook 'arv/ido-vertical-define-keys))

(customize-set-variable 'ido-enable-flex-matching t)
;;; (customize-set-variable 'ido-everywhere t)
(customize-set-variable 'ido-ignore-directories
 (quote ("\\`CVS/" "\\`\\.\\./" "\\`\\./" "\\`\\.svn/" "\\`\\.ropeproject/")))

;;; 'ido-mode.el ends here
