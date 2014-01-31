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
(require 'company)
(require 'pylint)
(require 'python)
(require 'hippie-exp)
(require 'he-utils)
(require 'highlight-indentation)

(require 'arv-py)

(eval-after-load "python"
  '(progn
     (modify-syntax-entry ?_ "w" python-mode-syntax-table)
     (setq python-shell-virtualenv-path (getenv "VIRTUAL_ENV"))
     ))

;; defineix advices per obrir/tancar un projecte rope al obrir/tancar
;; un projecte eproject
(eval-after-load "eproject"
  '(progn
    (defadvice eproject-open (after open-rope-project a)
      (if (file-exists-p (concat prj-directory ".ropeproject"))
          (rope-open-project prj-directory)))
    (defadvice eproject-close (before close-rope-project)
      (if (file-exists-p (concat prj-directory ".ropeproject"))
          (rope-close-project)))
    (ad-activate 'eproject-open)
    (ad-activate 'eproject-close)))


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
              (highlight-lines-matching-regexp "^[[:space:]]*import i?pdb")
              (highlight-lines-matching-regexp "^[[:space:]]*i?pdb.set_trace()")

              (highlight-lines-matching-regexp "sefl")

              ;; recicla la comanda 'compile' per executar els tests amb
              ;; nose
              (set (make-local-variable 'compile-command) "nosetests")

              ;; company-mode (complete anything)
              (company-mode t)
              (local-set-key (kbd "s-SPC") 'company-complete)

              ;; ressalta els nivell d'indentacio
              (highlight-indentation)

              ;; pylint/flymake-pylint
              (pylint-add-menu-items)
              (pylint-add-key-bindings)
              (local-set-key '[(super l)] 'pylint)
              (local-set-key '[f7] 'next-error)
              (local-set-key '[f8] 'previous-error)

              ;; local keymap
              (local-set-key '[(super tab)] 'python-indent-shift-right)
              (local-set-key '[(super iso-lefttab)] 'python-indent-shift-left)
              (local-set-key '[f9] (lambda ()
                                     (interactive)
                                     (call-interactively 'compile)))
              (local-set-key '[(control tab)]
                             (make-hippie-expand-function
                              '(try-expand-dabbrev
                                try-expand-tags
                                try-expand-dabbrev-all-buffers) t))
              (local-set-key '[(:)] 'arv-py-electric-colon)
              (local-set-key '[(control m)] 'newline-and-indent)
              (local-set-key '[(control c) (j) (i)] 'arv-py-nav-goto-first-import)))




;;; python-mode.el ends here
