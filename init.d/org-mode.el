;;; org-mode.el --- org-mode customization

;; $Id$


;;; Commentary:
;;


;;; History:
;;

;;; Local keybindings:
;;
;; M-<f1> visit the org reference card (PDF format) if available

;;; Code:

(defvar -refcard-directory-root (arv/path-join data-directory "refcards"))
(defvar -org-refcard (arv/path-join -refcard-directory-root "orgcard.pdf"))

(when (file-readable-p -org-refcard)
  (eval-after-load "org"
    '(progn
       (define-key org-mode-map (kbd "M-<f1>")
         (lambda () (interactive) (find-file -org-refcard))))))


;; define 'ecfg:' links
(eval-after-load "org"
  '(progn
     (org-add-link-type "ecfg" 'arv/org-ecfg-open)
     (add-hook 'org-store-link-functions 'arv/org-ecfg-store-link)
     (define-key org-mode-map (kbd "C-c M-l") 'arv/org-ecfg-insert-link-at-point)
     ))


;;; org-mode.el ends here
