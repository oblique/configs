;;; findstr.el --- Use Windows NT findstr to match expression in files

;; Copyright (C) 2000, 2003, 2005 by David Ponce

;; Author: David Ponce <david@dponce.com>
;; Maintainer: David Ponce <david@dponce.com>
;; Created: 21 Jul 2000
;; Keywords: tools
;; Revision: $Id: findstr.el,v 1.5 2005/07/05 07:00:36 ponced Exp $

(defconst findstr-version "1.1")

;; This file is not part of Emacs

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; This library use the Windows NT native command findstr to match
;; expressions in multiple files.
;;
;; To install and use, put this file on your Emacs-Lisp load path and
;; add the following into your ~/.emacs startup file:
;;
;; (autoload 'findstr "findstr"
;;   "Run Windows NT findstr to match expression in files." t)
;;

;;; History:
;;

;;; Code:
(require 'igrep)
(require 'compile)

;; The following default regexp take into account that Windows file
;; name can begin with a drive letter and contain spaces!
(defvar findstr-regexp-alist
  '(("^\\(\\([a-zA-Z]:\\)?[^:(\t\n]+\\)[:( \t]+\\([0-9]+\\)[:) \t]" 1 3))
  "Regexps to match the `findstr' entries.")

(defun findstr-internal (program expression files &optional options)
  "Use PROGRAM to find EXPRESSION in FILES.
PROGRAM is the native Windows NT findstr command.
OPTIONS specifies the findstr command options."
  (or (stringp options)
      (setq options ""))
  (or (listp files)
      (setq files (list files)))
  (let ((shell-file-name (getenv "ComSpec"))
        (shell-command-switch "/c")
        (w32-quote-process-args nil)
        (command (mapconcat #'identity
                            `(,program ,options ,expression ,@files)
                            " ")))
    (save-some-buffers (not compilation-ask-about-save) nil)
    (message "%s" command)
    (compile-internal command
                      (format "No more %s matches" program)
                      program
                      nil
                      findstr-regexp-alist)))

;;;###autoload
(defun findstr (&rest igrep-args)
  "Run Windows NT findstr to match expression in files.
All arguments IGREP-ARGS are handled by `findstr-internal'."
  (interactive
   (let ((igrep-program "findstr")
         (igrep-options "/n /s")
         (igrep-read-options nil))
     (igrep-read-args)))
  (apply 'findstr-internal igrep-args))

(provide 'findstr)

;;; findstr.el ends here
