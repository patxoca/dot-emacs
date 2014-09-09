;;; arv-org.el --- additional functionality for org-mode

;; $Id$

;; Emacs List Archive Entry
;; Filename: arv-org.el
;; Version: $Revision$
;; Keywords:
;; Author: Alexis Roda <alexis.roda.villalonga@gmail.com>
;; Maintainer: Alexis Roda <alexis.roda.villalonga@gmail.com>
;; Created: 2014-08-20
;; Description:
;; URL:
;; Compatibility: Emacs24

;; COPYRIGHT NOTICE
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more
;; details. http://www.gnu.org/copyleft/gpl.html

;;; Commentary:
;;

;;; History:
;;

(require 's)

(defvar arv-org-version-id
  "$Id$"
  "Latest modification time and version number.")


;;; 'ecfg:' links
;;
;; 'ecfg:' URLs point to files in my emacs configuration. The links
;; are relative to `emacs-startup-dir'. Useful for tasks/notes when
;; hacking my emacs configuration.

(defun arv/org-ecfg--relativize-to-startup-dir-maybe (path)
  "Converts .PATH into a path relative to
`emacs-startup-dir'. Returns the relative path or nil if PATH is
outside `emacs-startup-dir'.

PATH must be absolute."
  (let ((abs-startup-dir (file-name-as-directory (expand-file-name emacs-startup-dir))))
    (if (s-starts-with-p abs-startup-dir path)
        (s-chop-prefix abs-startup-dir path)
      nil)))

(defun arv/org-ecfg--link-at-point-get-range ()
  "Returns a list with the start and end position for the link at
point. A link is delimited by blanks and beggining/end of line."
  (let ((start (save-excursion
                 (skip-syntax-backward "^-" (line-beginning-position))
                 (point)))
        (end (save-excursion
               (skip-syntax-forward "^-" (line-end-position))
               (point))))
    (list start end)))

;;;###autoload
(defun arv/org-ecfg-open (path)
  "Visit the file within `emacs-startup-dir'."
  (find-file (arv/path-join emacs-startup-dir path)))

;;;###autoload
(defun arv/org-ecfg-store-link ()
  "Store a link to an emacs config file."
  (let ((link (arv/org-ecfg--relativize-to-startup-dir-maybe (buffer-file-name))))
    (when link
      (org-store-link-props
       :type "ecfg"
       :link (concat "ecfg:" link)
       :description (format "%s" link)))))

(defvar arv/org-ecfg-insert-link-at-point-history nil)

;;;###autoload
(defun arv/org-ecfg-insert-link-at-point (arg)
  "Insert a ecfg link using the text around the point. By default
the description part is the same as the text. Use the prefix
command in order to edit the description."
  (interactive "*P")
  (let* ((range (if mark-active
                    (list (point) (mark))
                  (arv/org-ecfg--link-at-point-get-range)))
         (text (apply 'buffer-substring-no-properties range))
         (link (if (s-starts-with-p "ecfg:" text)
                   text
                 (concat "ecfg:" text)))
         (description (if (s-starts-with-p "ecfg:" text)
                          (mapconcat 'identity (cdr (s-split ":" text)) ":")
                        text)))
    (if mark-active
        (deactivate-mark))
    (if arg
        (setq description (read-from-minibuffer "Description:" description
                                                nil nil
                                                'arv/org-ecfg-insert-link-at-point-history)))
    (apply 'delete-region range)
    (insert (format "[[%s][%s]]" link description))))

;;; state change and clock
;;
;; I want the clock to automatically start/stop clocking whenever a
;; task's state changes.
;;
;; Entering a state may trigger an action:
;;
;; - start: start clocking the current task. Clock-out the active task
;;   if any.
;;
;; - stop: stop clocking, but only if the current task is the active
;;   task. That's required in order to ensure that changing a task
;;   from a /paused/ state to other /paused/ state (from PAUSE to WAIT
;;   btw) does not clocks-out the active task,
;;
;; The mapping between states and actions is stored in the alist
;; `arv/org-sctc-entering-state-clocking-actions'. If an state is
;; missing or its associated action is nil then no action is performed
;; when it's entered.
;;
;; In case a task is being paused as a consequence of other being
;; started it will be put in the state defined in the variable
;; `arv/org-sctc-paused-state'.
;;
;; In order to enable that functionalitty call `arv/org-sctc-setup'
;; with no argument or nil. Passing an argument other than nil will
;; disable it.

(defvar arv/org-sctc-entering-state-clocking-actions nil
  "alist mapping states to actions.")

(defvar arv/org-sctc-paused-state nil
  "Stated for the task being paused.")


(defvar arv/org-sctc--previous-active-task-marker nil)

(defun arv/org-sctc--get-state-action (to)
  (cdr (assoc to arv/org-sctc-entering-state-clocking-actions)))

(defun arv/org-sctc--pause-other-task ()
  (when arv/org-sctc--previous-active-task-marker
    (unwind-protect
        (save-excursion
          (goto-char arv/org-sctc--previous-active-task-marker)
          ;; IMPORTANT: calling org-todo may produce infinite
          ;; recursion, be careful when changing the code!!
          (org-todo arv/org-sctc-paused-state))
      (progn
        (set-marker arv/org-sctc--previous-active-task-marker nil)
        (setq arv/org-sctc--previous-active-task-marker nil)
        (remove-hook 'post-command-hook 'arv/org-sctc--pause-other-task)))))

(defun arv/org-sctc--state-change (from to)
  (when (and (null arv/org-sctc--previous-active-task-marker)
             (not (string= from to)))
    (let ((action (arv/org-sctc--get-state-action to)))
      (unless (null action)
        (cond
         ((eq action 'start)
          (when (org-clock-is-active)
            (setq arv/org-sctc--previous-active-task-marker (copy-marker org-clock-marker))
            (org-clock-out)
            (add-hook 'post-command-hook 'arv/org-sctc--pause-other-task 'append))
          (org-clock-in))
         ((eq action 'stop)
          (let ((org-state "DONE") ;; hackish, review!!
                (org-clock-out-when-done t))
            (org-clock-out-if-current)))
         (t (user-error "Unknown action.")))))))

(defun arv/org-sctc--state-change-callback (p)
  (let ((type (plist-get p :type))
        (from (plist-get p :from))
        (to   (plist-get p :to)))
    (when (eq type 'todo-state-change)
      (arv/org-sctc--state-change from to))))

;;;###autoload
(defun arv/org-sctc-setup (&optional disable)
  (if disable
      (remove-hook 'org-trigger-hook 'arv/org-sctc--state-change-callback)
    (add-hook 'org-trigger-hook 'arv/org-sctc--state-change-callback)))


;;; Interruption handling
;;
;; I want to be able to track interruptions and resume interrupted
;; tasks.
;;
;; Execute `arv/org-interrupt-interrupt-active-task' to put the active
;; task in hold, pause the clock and start capturing.
;;
;; Execute `arv/org-interrupt-resume-last-active-task' to resume the
;; last interrupted task, if any.

(defvar arv/org-interrupt-resumed-state nil
  "State to put the task on when resuming after an
interruption.")

(defvar arv/org-interrupt-interrupted-state nil
  "State to put the task on when interrupted.")

(defvar arv/org-interrupt-capture-key nil
  "Key for the capture template used by the interruption.")

(defvar arv/org-interrupt--last-active-task nil
  "Private")

;;;###autoload
(defun arv/org-interrupt-interrupt-active-task ()
  "Interrupts the active task (if any) and starts capturing an
interruption.

If there's an active task its ID is saved in the variable
`arv/org-interrupt--last-active-task'."
  (interactive)
  (save-excursion
   (when (org-clock-is-active)
     (org-clock-goto)
     (setq arv/org-interrupt--last-active-task (org-id-get-create))
     (org-todo arv/org-interrupt-interrupted-state)))
  (org-capture nil arv/org-interrupt-capture-key))

;;;###autoload
(defun arv/org-interrupt-resume-last-active-task ()
  "If there's an interrupted task jump to it and start it again.

Currently this function relies on the value of the variable
`arv/org-interrupt--last-active-task' in order to locate the
interrupted task. The value of this variable won't survive
restarting emacs and won't be saved."
  (interactive)
  (when arv/org-interrupt--last-active-task
    (org-id-goto arv/org-interrupt--last-active-task)
    (setq arv/org-interrupt--last-active-task nil)
    (org-todo arv/org-interrupt-resumed-state)))


;;; assorted utilities

(autoload 'org-agenda-files "org" "" nil nil)

;;;###autoload
(defun arv/org-visit-agenda-file ()
  ""
  (interactive)
  (find-file
   (ido-completing-read "Agenda file:" (org-agenda-files) nil t)))


;;;###autoload
(defun arv/org-emphasize (char)
  (if mark-active
      (org-emphasize char)
    (insert char)))


(defun arv/org--get-tags ()
  (delete "" (split-string (or (org-entry-get (point) "TAGS") "") ":")))

(defun arv/org--get-parent-tags ()
  (save-excursion
    (save-restriction
      (widen)
      (if (org-up-heading-safe)
          (delete "" (split-string (or (org-entry-get (point) "ALLTAGS") "") ":"))
        nil))))

;;;###autoload
(defun arv/org-remove-reduntant-tags ()
  "Walks the tree-at-point removing redundant tags."
  (interactive)
  (when (eq major-mode 'org-mode)
    (save-excursion
      (org-map-entries
       (lambda ()
         (let ((tags    (arv/org--get-tags))
               (alltags (arv/org--get-parent-tags)))
           (dolist (tag tags)
             (when (member tag alltags)
               (org-toggle-tag tag 'off)))))
       nil 'tree)))
  ;; Something gets corrupted and tags are not displayed. That
  ;; happens in my real org file, in test.org (simplest) it works ok.
  ;; Collapsing works around the issue.
  (org-shifttab 2))

;;;###autoload
(defun arv/org-add-inherited-tags ()
  "Add inherited tags to sutree-at-point."
  (interactive)
  (when (eq major-mode 'org-mode)
    (save-excursion
      (org-back-to-heading)
      (let ((tags    (arv/org--get-tags))
            (alltags (arv/org--get-parent-tags)))
        (dolist (tag alltags)
          (unless (member tag tags)
            (org-toggle-tag tag 'on))))
      nil 'tree)))

;;;###autoload
(defun arv/org-refile (&rest args)
  "Like org-refile but updates tags."
  (interactive)
  (arv/org-add-inherited-tags)
  (apply 'org-refile args)
  (save-excursion
    (org-refile '(16))                 ;goto insertion
    (arv/org-remove-reduntant-tags)))


;;;###autoload
(defun arv/org-archive-subtree (&rest args)
  "Add inherited tags on archiving."
  (interactive)
  (arv/org-add-inherited-tags)
  (apply 'org-archive-subtree args))


;;;###autoload
(defun arv/org-start ()
  "Start org agenda within emacs server."
  (interactive)
  (server-start)
  (org-agenda nil "a"))


(provide 'arv-org)

;;; arv-org.el ends here
