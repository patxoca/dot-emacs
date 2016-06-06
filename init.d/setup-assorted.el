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


;; abbrev-mode
(eval-after-load "abbrev"
  '(progn
     (setq abbrev-file-name "~/.emacs.d/conf.d/shared/abbrev_defs")
     (setq save-abbrevs t)
     (setq default-abbrev-mode t)))

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


;; cursor
(eval-after-load "frame"
  '(progn
     (setq blink-cursor-blinks -1) ; cursor blinks forever
     ))


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
                    :height 109)


;; font-lock
(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)


;; hl-line-mode
(global-hl-line-mode t)
;;(customize-set-value 'hl-line-face 'highlight)
(set-face-attribute 'hl-line nil
                    :foreground "white"
                    :background "black")


;; multi-line
(require 'multi-line nil t)


;; overwrite mode
(defadvice overwrite-mode (after overwrite-mode-adjust-cursor activate)
  "Change cursor color when enabling/disabling overwrite mode."
  (set-cursor-color (if overwrite-mode
                        "purple"
                      "white")))


;; paradox
(setq paradox-github-token t)
(setq paradox-execute-asynchronously nil) ; t always, nil nevet, 'ask asks


;; pop-to-mark
;; http://endlessparentheses.com/faster-pop-to-mark-command.html
(eval-after-load "simple"
  '(progn
     (defadvice pop-to-mark-command (around ensure-new-position activate)
       (let ((p (point)))
         (dotimes (i 10)
           (when (= p (point)) ad-do-it))))
     (setq set-mark-command-repeat-pop t)))


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


;; which key
;; https://github.com/justbur/emacs-which-key
(when (require 'which-key nil t)
  (which-key-mode 1)
  (setq which-key-idle-delay 0.75)
  (setq which-key-sort-order 'which-key-description-order)
  (which-key-setup-side-window-right))


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

;; display-buffer-reuse-frames is a variable defined in `window.el'.
;; Its value is nil
;;
;;   This variable is obsolete since 24.3;
;;   use a `reusable-frames' alist entry in `display-buffer-alist'.
;;
;; Documentation:
;; Non-nil means `display-buffer' should reuse frames.
;; If the buffer in question is already displayed in a frame, raise
;; that frame.
(setq display-buffer-reuse-frames t)

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
