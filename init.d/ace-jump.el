;;; ace-jump.el --- customization for ace-jump

;; $Id$


;;; Commentary:
;;


;;; History:
;;


;;; Code:

;; Solves an issue with non ASCII chars. See:
;; https://github.com/winterTTr/ace-jump-mode/issues/38
;; https://github.com/winterTTr/ace-jump-mode/issues/38#issuecomment-23205670

(eval-after-load "ace-jump-mode"
  '(progn
     (defadvice ace-jump-char-category (around adv-ace-jump-support-umlauts activate)
       (unless (= (char-syntax (ad-get-arg 0)) ?w)
         ad-do-it)
       (setq ad-return-value 'alpha))))

;;; ace-jump.el ends here
