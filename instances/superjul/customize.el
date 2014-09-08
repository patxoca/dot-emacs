(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-auto-save t)
 '(TeX-parse-self t)
 '(TeX-printer-list
   (quote
    (("Local" "dvips %s -o; gv %s.ps" "lpq")
     ("lw")
     ("ps"))))
 '(ansi-color-for-comint-mode t)
 '(ansi-color-names-vector
   ["#3F3F3F" "#CC9393" "#7F9F7F" "#F0DFAF" "#8CD0D3" "#DC8CC3" "#93E0E3" "#DCDCCC"])
 '(browse-url-browser-function (quote browse-url-firefox))
 '(c-basic-offset 4)
 '(calendar-day-abbrev-array ["Dg" "Dl" "Dt" "Dc" "Dj" "Dv" "Ds"])
 '(calendar-day-name-array
   ["Diumenge" "Dilluns" "Dimarts" "Dimecres" "Dijous" "Divendres" "Dissabte"])
 '(calendar-month-name-array
   ["Gener" "Febrer" "Mar�" "Abril" "Maig" "Juny" "Juliol" "Agost" "Setembre" "Octubre" "Novembre" "Desembre"])
 '(calendar-week-start-day 1)
 '(column-number-mode t)
 '(company-backends
   (quote
    (company-elisp company-nxml company-css company-ropemacs company-files)))
 '(company-idle-delay 0.5)
 '(company-minimum-prefix-length 1)
 '(compilation-message-face (quote default))
 '(cperl-font-lock t)
 '(cperl-hairy t)
 '(cperl-indent-level 8)
 '(current-language-environment "UTF-8")
 '(custom-enabled-themes (quote (tango-dark)))
 '(custom-safe-themes
   (quote
    ("e16a771a13a202ee6e276d06098bc77f008b73bbac4d526f160faa2d76c1dd0e" "dd4db38519d2ad7eb9e2f30bc03fba61a7af49a185edfd44e020aa5345e3dca7" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(default-frame-alist (quote ((menu-bar-lines . 1) (tool-bar-lines . 0))))
 '(display-time-mode t nil (time))
 '(ecb-options-version "2.27")
 '(ecb-source-path (quote ("~/zope/develop")))
 '(ediff-split-window-function (quote split-window-horizontally))
 '(erc-insert-timestamp-function (quote erc-insert-timestamp-right))
 '(erc-server-coding-system (quote (latin-1 . latin-1)))
 '(erc-timestamp-format "[%H:%M]")
 '(erc-timestamp-right-column 0)
 '(erc-user-full-name "Alexis Roda")
 '(fci-rule-color "#383838")
 '(font-lock-maximum-decoration t)
 '(global-font-lock-mode t nil (font-lock))
 '(global-hl-line-mode t nil (hl-line))
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-tail-colors
   (quote
    (("#073642" . 0)
     ("#546E00" . 20)
     ("#00736F" . 30)
     ("#00629D" . 50)
     ("#7B6000" . 60)
     ("#8B2C02" . 70)
     ("#93115C" . 85)
     ("#073642" . 100))))
 '(hl-line-face (quote highlight))
 '(ido-enable-flex-matching t)
 '(ido-ignore-directories
   (quote
    ("\\`CVS/" "\\`\\.\\./" "\\`\\./" "\\`\\.svn/" "\\`\\.ropeproject/")))
 '(ido-save-directory-list-file "~/.emacs.d/ido.last")
 '(ido-ubiquitous-mode t)
 '(indent-tabs-mode nil)
 '(inhibit-eol-conversion nil)
 '(inhibit-startup-screen t)
 '(keyfreq-autosave-mode t)
 '(keyfreq-file "~/.emacs.d/keyfreq")
 '(keyfreq-file-lock "~/.emacs.d/keyfreq.lock")
 '(keyfreq-mode t)
 '(lpr-command "lp")
 '(lpr-printer-switch "-d")
 '(menu-bar-mode nil)
 '(mouse-buffer-menu-maxlen 25)
 '(mouse-buffer-menu-mode-mult 10)
 '(mouse-wheel-down-button 4 t)
 '(mouse-wheel-follow-mouse t)
 '(mouse-wheel-mode t nil (mwheel))
 '(mouse-wheel-scroll-amount (quote (1 5)))
 '(mouse-yank-at-point t)
 '(next-line-add-newlines nil)
 '(org-directory "~/.emacs.d/org")
 '(org-startup-folded t)
 '(org-todo-keywords (quote ((sequence "TODO" "INPROCESS" "DONE"))))
 '(package-archives
   (quote
    (("marmalade" . "http://marmalade-repo.org/packages/")
     ("gnu" . "http://elpa.gnu.org/packages/")
     ("melpa stable" . "http://melpa-stable.milkbox.net/packages/"))))
 '(printer-name "hp1220c")
 '(ps-n-up-printing 1)
 '(ps-printer-name-option "-d")
 '(ps-zebra-stripes nil)
 '(py-indent-offset 4)
 '(py-prepare-autopair-mode-p nil)
 '(py-shell-name "ipython")
 '(py-smart-indentation nil)
 '(python-indent-guess-indent-offset nil)
 '(python-pep8-options (quote ("--repeat" "--ignore=E203,E201,E202,E123")))
 '(ropemacs-enable-autoimport t)
 '(safe-local-variable-values
   (quote
    ((python-shell-prompt-output-regexp . "Out\\[[0-9]+\\]: ")
     (python-shell-prompt-regexp . "In \\[[0-9]+\\]: ")
     (ispell-local-dictionay . "british")
     (ispell-local-dictionay . british)
     (python-shell-interpreter-args . "/home/alex/prog/practicum2/trunk/project/manage.py shell")
     (python-shell-interpreter-args . "/home/alex/prog/practicum2/project/manage.py shell")
     (python-shell-completion-string-code . "';'.join(get_ipython().Completer.all_completions('''%s'''))
")
     (python-shell-completion-module-string-code . "';'.join(module_completion('''%s'''))
")
     (python-shell-completion-setup-code . "from IPython.core.completerlib import module_completion")
     (python-shell-interpreter-args . "/home/alex/prog/practicum2/curs1415/project/manage.py shell")
     (python-shell-interpreter . "python")
     (ispell-dictionary . british)
     (ispell-local-dictionary . british)
     (encoding . utf-8))))
 '(scroll-bar-mode nil)
 '(scroll-preserve-screen-position t)
 '(scroll-step 1)
 '(show-paren-mode t)
 '(show-trailing-whitespace nil)
 '(srctool-todo-ignore-directories (quote (".emacs~" "CVS" "RCS" ".svn" "epydoc")))
 '(srctool-todo-labels (quote ("@TODO:" "@FIXME:" "@NOTE:" "@FIXME210:")))
 '(svn-status-default-log-arguments (quote ("-v" "--stop-on-copy")))
 '(svn-status-verbose nil)
 '(tab-width 4)
 '(template-auto-insert t)
 '(template-confirm-insecure nil)
 '(time-stamp-format "%a, %02d/%02m/%02y %02H:%02M:%02S")
 '(tool-bar-mode nil nil (tool-bar))
 '(tooltip-mode t nil (tooltip))
 '(transient-mark-mode t)
 '(uniquify-buffer-name-style (quote reverse) nil (uniquify))
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-color-map
   (quote
    ((20 . "#BC8383")
     (40 . "#CC9393")
     (60 . "#DFAF8F")
     (80 . "#D0BF8F")
     (100 . "#E0CF9F")
     (120 . "#F0DFAF")
     (140 . "#5F7F5F")
     (160 . "#7F9F7F")
     (180 . "#8FB28F")
     (200 . "#9FC59F")
     (220 . "#AFD8AF")
     (240 . "#BFEBBF")
     (260 . "#93E0E3")
     (280 . "#6CA0A3")
     (300 . "#7CB8BB")
     (320 . "#8CD0D3")
     (340 . "#94BFF3")
     (360 . "#DC8CC3"))))
 '(vc-annotate-very-old-color "#DC8CC3")
 '(w3m-session-crash-recovery nil)
 '(zencoding-indentation 2))
