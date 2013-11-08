;;; python-mode.el --- personalitzacio del python mode
;;; (c) 2006 Alexis Roda
;;; $Id$

;;; Commentary:
;;;
;;;  arxiu de configuracio de python-mode

;;; History:
;;;
;;; consultar log de svn

;;; Code:

(require 'autopair)
(require 'python)
(require 'hippie-exp)
(require 'he-utils)
(require 'highlight-indentation)
(require 'smart-operator)

(require 'arv-py)

(defun arv-smart-operator-: ()
  "El keymap de `smart-operator' oculta el keybinding local de
':'. Aquesta funció permet obtindre el comportament desitjat en
`python-mode', mantenint el comportament original en la resta de
modes.

La suposició de que el comportament original és el definit per la
funció `smart-operator' és una mica restrictiu. Caldria recuperar
la funció actualment associada a ':'."
  (interactive)
  (if (eq major-mode 'python-mode)
      (call-interactively 'arv-py-electric-colon)
    (smart-operator-:)))
(define-key smart-operator-mode-map (kbd ":") 'arv-smart-operator-:)

(add-hook 'python-mode-hook
          #'(lambda ()
              ;; outline
              (outline-minor-mode t)
              (set
               (make-local-variable 'outline-regexp)
               "[\t ]*\\(class\\|def\\|if\\|elsif\\|else\\|while\\|for\\|try\\|except\\|finally\\)\\>")

              ;; autopair
              (setq autopair-handle-action-fns
                    (list #'autopair-default-handle-action
                          #'autopair-python-triple-quote-action))

              ;; ressalta les línies on hi ha un breakpoint
              (highlight-lines-matching-regexp "import pdb")
              (highlight-lines-matching-regexp "pdb.set_trace()")

              ;; recicla la comanda 'compile' per executar els tests amb
              ;; nose
              (set (make-local-variable 'compile-command) "nosetests")

              ;; company-mode (complete anything)
              (company-mode t)
              ;; ressalta els nivell d'indentacio
              (highlight-indentation)
              (smart-operator-mode 1)
              ;; local keymap
              (local-set-key '[(super tab)] 'python-indent-shift-right)
              (local-set-key '[(super iso-lefttab)] 'python-indent-shift-left)
              (local-set-key '[f7] 'flymake-goto-next-error)
              (local-set-key '[f8] 'flymake-goto-prev-error)
              (local-set-key '[f9] (lambda ()
                                     (interactive)
                                     (call-interactively 'compile)))
              (local-set-key '[(control tab)]
                             (make-hippie-expand-function
                              '(try-expand-dabbrev
                                try-expand-tags
                                try-expand-dabbrev-all-buffers) t))
              (local-set-key '[(:)] 'arv-py-electric-colon)
              (local-set-key '[(control m)] 'newline-and-indent)))




;;; python-mode.el ends here
