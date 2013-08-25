;;; pylint-flymake.el --- personalitzacio del python mode
;;; (c) 2012 Alexis Roda
;;; $Id$

;;; Commentary:
;;;
;;; arxiu per integrar 'pylint' amb 'flymake'
;;;
;;; http://emacswiki.org/emacs/PythonProgrammingInEmacs#toc8

;;; History:
;;;
;;; consultar log de svn

;;; Code:


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


(add-hook 'python-mode-hook
          #'(lambda ()
              (local-set-key '[(super l)] 'flymake-mode)
))

;;; pylint-flymake.el ends here
