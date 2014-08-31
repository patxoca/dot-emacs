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

(autoload 'company-complete "company" "" t nil)
(autoload 'next-error "simple" "" t nil)
(autoload 'previous-error "simple" "" t nil)
(autoload 'highlight-indentation "highlight-indentation" "" t nil)

(require 'arv-py)


;;                              pymacs

(require 'pymacs)

(defun fp-maybe-pymacs-reload ()
  "Recarrega automaticament els arxius .py guardats dins ~/emacslib."

  ;; @TODO: alex 2006-08-15 00:28:32 : no recordo el perque d'aquesta
  ;; funcio, sembla una mena de reload al guardar, pero ignora el que
  ;; hi ha dins de ~/prog, que tambe apareix a pymacs-load-path.
  ;;
  ;; Crec que le problema de ~/prog es que hi han coses que no tenen
  ;; res a veure amb pymacs i per aixo l'ignoro, pero entonces esta
  ;; funcio perd tota utilitat, no escric python dins emacslib!!
  ;;
  ;; Per fer-ho ben fet caldria fer customizable la variable
  ;; pymacs-load-path i tindre en compte tots els elements de la
  ;; llista al determinar si cal que pymacs recarregui l'arxiu

  (let ((pymacsdir (expand-file-name emacs-startup-dir)))
    (when (and (string-equal (file-name-directory buffer-file-name)
                             pymacsdir)
               (string-match "\\.py\\'" buffer-file-name))
      (princ (concat "pymacs: recarregant " buffer-file-name))
      (pymacs-load (substring buffer-file-name 0 -3)))))

;; (add-hook 'after-save-hook 'fp-maybe-pymacs-reload)

(autoload 'pymacs-apply "pymacs")
(autoload 'pymacs-call "pymacs")
(autoload 'pymacs-eval "pymacs" nil t)
(autoload 'pymacs-exec "pymacs" nil t)
(autoload 'pymacs-load "pymacs" nil t)
(autoload 'pymacs-autoload "pymacs")
(eval-after-load "pymacs"
  '(add-to-list 'pymacs-load-path (arv/startup-get-absolute-path "shared/pymacs")))


;;                     rope refactoring library

(eval-after-load "pymacs"
  '(progn
     (message "Loading ropemacs ...")
     (condition-case ex
         (progn
           (pymacs-load "ropemacs" "rope-")
           (message "ropemacs loaded"))
       ('error (message "ropemacs failed")))))


;;                         nose test runner

(eval-after-load "python"
  '(progn
     (require 'nose-mode)
     (nose-mode-setup-keymap)
     (add-hook 'python-mode-hook
               'nose-mode-enable-if-test-module)))


;;                             pylookup

(autoload 'pylookup-lookup "pylookup")
(autoload 'pylookup-update "pylookup")

(eval-after-load "python"
  '(progn
     (define-key python-mode-map [(control c) (h)] 'pylookup-lookup)))

(add-hook 'python-mode-hook
          (lambda()
            (let ((pylookup-dir (arv/startup-get-absolute-path "site-lisp/pylookup")))
              (setq pylookup-program (arv/path-join pylookup-dir "pylookup.py"))
              (setq pylookup-db-file (arv/path-join pylookup-dir "pylookup.db"))
              )))


;;                             flymake

(when (load "flymake" t)
  (defun flymake-pylint-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "epylint" (list local-file))))

  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pylint-init)))


;;                            pydoc info

(autoload 'info-lookup-add-help "info-look" "" nil nil)

(eval-after-load "python"
  (info-lookup-add-help
   :mode 'python-mode
   :parse-rule 'pydoc-info-python-symbol-at-point
   :doc-spec
   '(("(python)Index" pydoc-info-lookup-transform-entry)
     ("(django14)Index" pydoc-info-lookup-transform-entry))))


;;                              python

(eval-after-load "python"
  '(progn
     (modify-syntax-entry ?_ "w" python-mode-syntax-table)
     (setq python-shell-virtualenv-path (getenv "VIRTUAL_ENV"))

     ;; keybindings locals
     (define-key python-mode-map (kbd "s-SPC") 'company-complete)
     (define-key python-mode-map '[(super l)] 'pylint)
     (define-key python-mode-map '[(super L)] 'pep8)
     (define-key python-mode-map '[(meta f1)] (lambda () (interactive) (info "(python.info) Python Module Index")))
     (define-key python-mode-map '[f7] 'next-error)
     (define-key python-mode-map '[f8] 'previous-error)
     (define-key python-mode-map '[f9] (lambda ()
                                         (interactive)
                                         (call-interactively 'compile)))
     (define-key python-mode-map '[(control tab)]
       (make-hippie-expand-function
        '(try-expand-dabbrev
          try-expand-tags
          try-expand-dabbrev-all-buffers) t))
     (define-key python-mode-map '[(super tab)] 'python-indent-shift-right)
     (define-key python-mode-map '[(super iso-lefttab)] 'python-indent-shift-left)
     (define-key python-mode-map '[(:)] 'arv-py-electric-colon)
     (define-key python-mode-map '[(control m)] 'newline-and-indent)
     (define-key python-mode-map '[(control c) (j) (i)] 'arv-py-nav-goto-first-import)))


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
          (lambda ()
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

            ;; company-mode
            (company-mode t)

            ;; ressalta els nivell d'indentacio
            (highlight-indentation)

            ;; pylint/flymake-pylint
            (pylint-add-menu-items)
            (pylint-add-key-bindings)))


;;; python-mode.el ends here