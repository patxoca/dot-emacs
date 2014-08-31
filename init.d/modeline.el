;;; modeline.el --- mode line customization

;; $Id$


;;; Commentary:
;;

;;; Local keybindings:
;;
;; C-M-q: does some amazing stuff

;;; History:
;;


;;; Code:

(require 'diminish)
(require 's)


;; List of modes to be diminished. Each item has the format:
;;
;; (MODE PACKAGE REPLACEMENT)
;;
;; - MODE is required
;; - PACKAGE defaults to MODE without the '-mode' suffix.
;; - REPLACEMENT defaults to ""
(setq arv/modeline--diminished-modes
      '((autopair-mode)
        (company-mode)
        (eldoc-mode)
        (elisp-slime-nav-mode)
        (google-this-mode)
        (guide-key-mode)
        (hi-lock-mode)
        (org-capture-mode nil " C")
        (outline-minor-mode "outline")
        (paredit-mode)
        (rainbow-mode "rainbow-mode")
        (ropemacs-mode "python-mode")
        (workgroups-mode)
        (yas-minor-mode "yasnippet")
        (zencoding-mode "zencoding-mode")))

(defun arv/modeline--diminish-modes ()
  (dolist (item arv/modeline--diminished-modes)
    (let ((mode (car item))
          (package (or (cadr item)
                       (s-chop-suffix "-mode" (symbol-name (car item)))))
          (replacement (or (caddr item) "")))
      ;; diminishing must be delayed until the corresponding package
      ;; is loaded and the minor mode, hopefully, gets defined.
      (eval-after-load package
        (list 'diminish (list 'quote mode) replacement)))))


;; hide line and column number
(column-number-mode 0)
(line-number-mode 0)

;; hide time and system load
(display-time-mode 0)

;; diminish selected modes
(arv/modeline--diminish-modes)

;;; modeline.el ends here
