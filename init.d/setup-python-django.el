;;; python-django.el --- inicialització per python-django

;; $Id$


;;; Commentary:
;;

;;; History:
;;


;;; Code:

(autoload 'pony-tpl-mode "pony-tpl" "" t nil)
(autoload 'arv/django-file-belongs-to-app-p "arv-python-django" "" t nil)

;; configuració dels modes involucrats

(eval-after-load "python-django"
  '(progn
     (define-key python-django-mode-map (kbd "<left>") 'python-django-ui-move-up-tree)))

(eval-after-load "python"
  '(progn
     (define-key python-mode-map (kbd "C-c j t") 'arv/django-jump-to-template)
     (define-key python-mode-map (kbd "C-c j j") 'arv/django-jump-to-javascript-controller)))

(eval-after-load "sgml-mode"
  '(progn
     (define-key html-mode-map (kbd "C-c j t") 'arv/django-jump-to-template)
     (define-key html-mode-map (kbd "%") 'arv/django-autopair-template-tag)
     (add-hook 'html-mode-hook
               (lambda ()
                 (when (arv/django-file-belongs-to-app-p buffer-file-name)
                   (pony-tpl-mode))))))

;;; 'python-django.el ends here
