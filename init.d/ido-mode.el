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
  (define-key ido-completion-map (kbd "<right>") 'ido-next-match-dir)
  (define-key ido-completion-map (kbd "M-<left>") 'ido-prev-work-directory)
  (define-key ido-completion-map (kbd "M-<right>") 'ido-next-work-directory)
  )



(ido-mode 1)
(ido-everywhere 1)

(when (require 'ido-vertical-mode nil 'noerror)
  (ido-vertical-mode 1)
  (add-hook 'ido-setup-hook 'arv/ido-vertical-define-keys))

;; Inhibit switching to other directory when no matches are found in
;; the current directory.
(customize-set-variable 'ido-auto-merge-work-directories-length -1)
(customize-set-variable 'ido-enable-flex-matching t)
(customize-set-variable 'ido-ignore-directories
 (quote ("\\`CVS/" "\\`\\.\\./" "\\`\\./" "\\`\\.svn/" "\\`\\.ropeproject/")))
(customize-set-variable 'ido-max-work-directory-list 10)

;;; 'ido-mode.el ends here
