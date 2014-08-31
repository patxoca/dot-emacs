;;; setup-guide-key.el --- customization for guide-key

;; $Id$


;;; Commentary:
;;

;;; History:
;;


;;; Code:

(when (require 'guide-key nil t)
  (setq guide-key/guide-key-sequence '("C-c" "C-h" "C-x r" "C-x 4"))
  (setq guide-key/recursive-key-sequence-flag t)
  (setq guide-key/idle-delay 0.75)
  (setq guide-key/popup-window-position 'bottom)
  (setq guide-key/align-command-by-space-flag t)
  (setq guide-key/text-scale-amount -1)
  (set-face-attribute 'guide-key/highlight-command-face nil
                      :foreground "white"
                      :weight 'bold)
  (guide-key-mode 1)

  (eval-after-load "org"
    '(progn
       (add-hook 'org-mode-hook
                 (lambda ()
                   (guide-key/add-local-highlight-command-regexp "org-")))))

  (eval-after-load "python"
    '(progn
       (add-hook 'python-mode-hook
                 (lambda ()
                   (guide-key/add-local-highlight-command-regexp "python-"))))))

;;; setup-guide-key.el ends here
