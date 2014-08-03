;;; ensure-installed-packages.el --- garanteix que els paquests estan instal.lats

;; $Id$


;;; Commentary:
;;


;;; History:
;;


;;; Code:


(defvar required-packages
  '(
    ace-jump-mode
    buffer-move
    company
    dash
    elisp-slime-nav
    expand-region
    feature-mode
    figlet
    find-file-in-project
    grin
    highlight-indentation
    ido-ubiquitous
    ido-vertical-mode
    js2-mode
    json-mode
    keywiz
    lorem-ipsum
    macrostep
    magit
    multiple-cursors
    nose
    nose-mode
    org
    paredit
    php-mode
    pretty-lambdada
    pysmell
    pep8
    rainbow-mode
    s
    smart-operator
    smex
    solarized-theme
    tango-2
    workgroups
    yasnippet
    zencoding-mode
    )
  "List of required packages installable from ELPA/Marmalade.")


(defun -package-p (name)
  "Return t if NAME is an available package."
  (unless package-archive-contents
    (package-refresh-contents))
  (not (null (assoc name package-archive-contents))))

(defun -arv/require-package (pkg)
  (when (and (-package-p pkg)
             (not (package-installed-p pkg)))
    (package-install pkg)))


;;;###autoload
(defun arv/ensure-required-packages ()
  ""
  (interactive)
  (dolist (p required-packages)
    (-arv/require-package p)))

;;; 00-ensure-packages.el ends here
