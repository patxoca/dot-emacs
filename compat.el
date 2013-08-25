;;; compat.el --- funcions de compatibilitat

;;; Commentary:
;; funcions de compatibilitat amb altres versions/sabors d'emacs


;;; History:
;;

;;; Code:

;; funcio definida a emacs 22, es necessita per msf-abbrev
;; http://cvs.savannah.gnu.org/viewcvs/emacs/lisp/simple.el?rev=1.832&root=emacs&view=auto

(unless (fboundp 'line-number-at-pos)
  (defun line-number-at-pos (&optional pos)
    "Return (narrowed) buffer line number at position POS.
If POS is nil, use current buffer location.
Counting starts at (point-min), so the value refers
to the contents of the accessible portion of the buffer."
    (let ((opoint (or pos (point))) start)
      (save-excursion
        (goto-char (point-min))
        (setq start (point))
        (goto-char opoint)
        (forward-line 0)
        (1+ (count-lines start (point)))))))

;; I found that this function is not available in the latest
;; etags.el. Here is what I put in my .emacs
;;
;; – Sudheer Koganti
;; http://emacswiki.org/emacs/HippieExpand

(defun tags-complete-tag (string predicate what)
  (save-excursion
    ;; If we need to ask for the tag table, allow that.
    (if (eq what t)
        (all-completions string (tags-completion-table) predicate)
      (try-completion string (tags-completion-table) predicate))))


;;; compat.el ends here
