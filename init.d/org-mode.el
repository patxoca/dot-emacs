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



;;; org-mode.el ends here
