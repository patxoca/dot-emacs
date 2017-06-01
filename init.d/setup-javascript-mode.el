;;; javascript-mode.el --- personalitzacio del javascript mode
;;; (c) 2006 Alexis Roda
;;; $Id$

;;; Commentary:
;;;
;;; Arxiu de configuracio de javascript-mode


;;; History:
;;;
;;; consultar log de svn

;;; Code:


(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

(with-eval-after-load "js2-mode"

  ;; js2-mode

  (add-hook 'js2-mode-hook #'js2-imenu-extras-mode)

  ;; js2-refactor

  (add-hook 'js2-mode-hook #'js2-refactor-mode)
  (js2r-add-keybindings-with-prefix "C-c m r")
  (define-key js2-mode-map (kbd "C-k") #'js2r-kill)

  ;; xref-js2

  (define-key js-mode-map (kbd "M-.") nil)
  (add-hook 'js2-mode-hook
            (lambda () (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))

  ;; company-node

  (add-to-list 'company-backends 'company-tern)
  (add-hook 'js2-mode-hook (lambda ()
                             (tern-mode)
                             (company-mode)))

  ;; Disable completion keybindings, as we use xref-js2 instead

  (define-key tern-mode-keymap (kbd "M-.") nil)
  (define-key tern-mode-keymap (kbd "M-,") nil))

;;; javascript-mode.el ends here
