;;; grin.el --- emacs interface to grin -*- lexical-binding: t -*-

;; Author: Alexis Roda
;; Maintainer: Alexis Roda
;; Version: 0.1
;; Package-Requires: (transient)
;; Homepage: homepage
;; Keywords: keywords


;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.


;;; Commentary:


;;; Code:

(require 'transient)

(defvar grin--hist nil)

(defgroup grin nil
  "Run grin putting hits in a grep buffer."
  :group 'tools
  :group 'processes)

(defcustom grin-cmd "grin"
  "The grin command."
  :type 'string
  :group 'grin)


(transient-define-argument grin:--skip-exts ()
  :description "list of extensions to skip"
  :class 'transient-option
  :shortarg "-e"
  :argument "--skip-exts=")

(transient-define-argument grin:--skip-dirs ()
  :description "list of directories to skip"
  :class 'transient-option
  :shortarg "-d"
  :argument "--skip-dirs=")

(transient-define-argument grin:--include ()
  :description "find in files matching glob"
  :class 'transient-option
  :shortarg "-I"
  :argument "--include=")

(transient-define-argument grin:--files-from-file ()
  :description "file to read filenames from"
  :class 'transient-option
  :shortarg "-f"
  :argument "--files-from-file=")

(defun grin--run (regex directory &optional args)
  "Run the grin command on DIRECTORY looking for REGEX."
  (interactive (list
                (read-string "Regex: ")
                (ido-read-directory-name "Directory: " nil nil t)
                (transient-args 'arv/grin)))
  (let ((cmd (concat grin-cmd " --emacs " (mapconcat #'identity args " ") " " regex))
        (default-directory directory)
        (null-device nil))
    (grep cmd)))

;;;###autoload (autoload 'arv/grin "grin" nil t)
(transient-define-prefix arv/grin ()
  "Run grin."
  :value '("-i")
  ["Arguments (C-x l for extra options)"
   (1 "-i" "Ignore case" "-i")
   (5 "-s" "do NOT skip .hidden files" "-s")
   (5 "-S" "do NOT skip .hidden dirs" "-S")
   (5 "-F" "follow symlinks" "--follow")
   (1 grin:--skip-exts)
   (5 grin:--skip-dirs)
   (1 grin:--include)
   (5 grin:--files-from-file)]
  [["Commands"
    ("x" "Execute" grin--run)]])

;;;###autoload (autoload 'grin "grin" nil t)
(defun grin ()
  (interactive)
  (let* ((c (concat grin-cmd " --emacs \"\""))
         (l (length c))
         (default-directory (ido-read-directory-name "Directory: " nil nil t))
         (cmd (read-shell-command "Command: " (cons c l) 'grin-hist))
         (null-device nil))
    (grep cmd)))


(provide 'grin)

;;; grin.el ends here
