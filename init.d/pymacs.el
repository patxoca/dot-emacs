;;; pymacs.el --- personalitzacio de pymacs
;;; (c) 2006 Alexis Roda
;;; $Id$

;;; Commentary:
;;;
;;; Arxiu de configuracio de pymacs.  Pymacs permet utilitzar python
;;; com llenguatge d'extensio d'Emacs
;;;

;;; History:
;;;
;;; consultar log de svn

;;; Code:

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

;;; pymacs.el ends here
