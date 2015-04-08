;;; assorted.el --- assorted customization

;; $Id$


;;; Commentary:
;;
;; Customizations that don't fit anywhere else and are so simple to
;; deserve its own module.

;;; History:
;;


;;; Code:


(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Change window title
(when window-system
  (setq frame-title-format '("emacs %b (%f)")))


;; ace-window-mode
(eval-after-load "ace-window"
  '(progn
     (custom-set-faces
      '(aw-leading-char-face
        ((t (:inherit ace-jump-face-foreground
                      :height 4.0
                      :foreground "red")))))
     (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l ?ñ))))


;; company
(eval-after-load "company"
  '(progn
     (setq company-minimum-prefix-length 1)
     (setq company-idle-delay 0.5)
     (setq company-backends '(company-elisp
                              company-nxml
                              company-css
                              company-ropemacs
                              company-files))))


;; ediff
(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)


;; emacs faces
(set-face-attribute 'compilation-error nil
                    :underline nil
                    :weight 'normal)
(set-face-attribute 'cursor  nil
                    :background "white")
(set-face-attribute 'default nil
                    :height 113)


;; font-lock
(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)


;; hl-line-mode
(global-hl-line-mode t)
;;(customize-set-value 'hl-line-face 'highlight)
(set-face-attribute 'hl-line nil
                    :foreground "white"
                    :background "black")


;; overwrite mode
(defadvice overwrite-mode (after overwrite-mode-adjust-cursor activate)
  "Change cursor color when enabling/disabling overwrite mode."
  (set-cursor-color (if overwrite-mode
                        "purple"
                      "white")))


;; paradox
(setq paradox-github-token t)
(setq paradox-execute-asynchronously nil) ; t always, nil nevet, 'ask asks


;; popwin
(require 'popwin)
(popwin-mode 1)


;; Non-nil means a single space does not end a sentence. This is
;; relevant for filling.
(setq sentence-end-double-space nil)


;; smex
(eval-after-load "smex"
  '(progn
     (setq smex-save-file (arv/path-join user-emacs-directory "smex-items"))
     (smex-initialize)))


;; srctool
(eval-after-load "todo"
  '(progn
     (setq srctool-todo-ignore-directories '(".emacs~" "CVS" "RCS" ".svn" "epydoc"))
     (setq srctool-todo-labels '("@TODO:" "@FIXME:" "@OPTIMIZE:" "@HACK:" "@REVIEW:" "@NOTE:"))))


;; uniquify
(eval-after-load "uniquify"
  '(progn
     (setq uniquify-buffer-name-style 'reverse)))


;; web browser
(setq browse-url-browser-function 'browse-url-generic)
(setq browse-url-generic-program "firefox")


(mouse-wheel-mode t)
;;@@ (setq mouse-wheel-scroll-amount'(1 5))
(setq mouse-wheel-follow-mouse t)
;;@@ (setq mouse-wheel-down-event 4)
;;@@ (setq mouse-buffer-menu-mode-mult 10)
;;@@ (setq mouse-buffer-menu-maxlen 25)

;; assorted
(setq ansi-color-for-comint-mode t)
(setq calendar-day-abbrev-array ["Dg" "Dl" "Dt" "Dc" "Dj" "Dv" "Ds"])
(setq calendar-day-name-array ["Diumenge" "Dilluns" "Dimarts" "Dimecres"
                               "Dijous" "Divendres" "Dissabte"])
(setq calendar-month-name-array ["Gener" "Febrer" "Març" "Abril" "Maig"
                                 "Juny" "Juliol" "Agost" "Setembre"
                                 "Octubre" "Novembre" "Desembre"])
(setq calendar-week-start-day 1)
(setq compilation-message-face 'default)
(setq current-language-environment "UTF-8")
(setq-default indent-tabs-mode nil)
(setq inhibit-eol-conversion nil)
(setq inhibit-startup-screen t)
(setq mouse-buffer-menu-maxlen 25)
(setq mouse-buffer-menu-mode-mult 10)
(setq mouse-yank-at-point t)
(setq next-line-add-newlines nil)
(setq scroll-preserve-screen-position t)
(setq scroll-step 1)
(setq show-paren-mode t)
(setq svn-status-default-log-arguments '("-v" "--stop-on-copy"))
(setq svn-status-verbose nil)
(setq-default tab-width 4)
(setq text-scale-mode-step 1.1)
(setq tooltip-mode t)
(setq transient-mark-mode t)
(setq visible-bell t)
;;@@ (setq time-stamp-format "%a, %02d/%02m/%02y %02H:%02M:%02S") ;; not sure
;;@@ (setq default-frame-alist (quote ((menu-bar-lines . 1) (tool-bar-lines . 0))))

(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)


;;; assorted.el ends here
