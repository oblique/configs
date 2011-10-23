;;; gdiff.el -- Use a GUI diff tool from Emacs

;; Copyright (C) 2002, 2003, 2005 by David Ponce

;; Author: David Ponce <david@dponce.com>
;; Maintainer: David Ponce <david@dponce.com>
;; Created: Aug 21 2002
;; Keywords: tools
;; Revision: $Id: gdiff.el,v 1.5 2005/07/05 07:00:36 ponced Exp $

(defconst gdiff-version "1.0")

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
;; This library manage a GUI diff tool from Emacs, allowing the
;; following commands:
;;
;; `gdiff-buffer-with-file'
;;    Compare a buffer and its associated file.
;;
;; `gdiff-files'
;;    Compare 2 files.
;;
;; `gdiff-folders'
;;    Compare 2 folders.
;;
;; `gdiff-revision'
;;    Compare 2 revisions of a file under version control.
;;
;; `gdiff-latest-revision'
;;    Compare working copy and latest revision of a file under version
;;    control.
;;
;; Installation:
;;
;; gdiff.el requires vc.el to compare revisions.
;;
;; Put this file and gdiff-setup.el on your Emacs-Lisp load path and
;; add (require 'gdiff-setup) into your ~/.emacs startup file.
;;
;; If possible gdiff-setup adds a "Compare (GUI)" sub-menu in the
;; "Tools" menu.
;;

;;; History:
;;

;;; Code:
(require 'gdiff-setup)
(require 'vc)

;;; Compatibility
;;
(if (fboundp 'subst-char-in-string)
    
    (defalias 'gdiff-subst-char-in-string 'subst-char-in-string)
  
  ;; Copied from GNU Emacs subr.el
  (defun gdiff-subst-char-in-string (fromchar tochar string &optional inplace)
    "Replace FROMCHAR with TOCHAR in STRING each time it occurs.
Unless optional argument INPLACE is non-nil, return a new string."
    (let ((i (length string))
          (newstr (if inplace string (copy-sequence string))))
      (while (> i 0)
        (setq i (1- i))
        (if (eq (aref newstr i) fromchar)
            (aset newstr i tochar)))
      newstr))
  
  )

(defun gdiff-vc-version (rev)
  "Retrieve the revision REV of the current buffer file.
Return the workfile name. See also `vc-version-other-window'."
  (let (filename)
    (save-window-excursion
      (save-excursion
        (vc-version-other-window rev)
        (setq filename (buffer-file-name))
        (kill-buffer (current-buffer))))
    filename))

(defun gdiff-vc-internal (rev1 rev2)
  "Compare revisions REV1 and REV2 of the current buffer file.
If REV1 is \"\" it defaults to the workfile version.  If REV2 is \"\"
the current buffer working copy is compared with REV1."
  (vc-ensure-vc-buffer)
  (let ((file2 (if (string= rev2 "")    ; use the buffer working copy
                   (gdiff-buffer-file-name)
                 (gdiff-vc-version rev2)))
        (file1 (gdiff-vc-version rev1)))
    (gdiff-execute-nowait file1 file2
                          `(lambda ()   ; cleanup temp working files
                             (or (string= ,file1 ,(buffer-file-name))
                                 (gdiff-delete-file ,file1))
                             (or (string= ,file2 ,(buffer-file-name))
                                 (gdiff-delete-file ,file2))))))

(defvar gdiff-cleanup-function nil
  "CLeanup function runs after the gdiff process has terminated.
The cleanup function has no arguments.  This variable is locally
defined in each process buffer.")
(make-variable-buffer-local 'gdiff-cleanup-function)

(defun gdiff-execute-nowait (f1 f2 &optional cleanup)
  "Run `gdiff-program' asynchronously to compare files F1 and F2.
CLEANUP specify an optional function called after the gdiff process
has terminated.  Additional command line parameters defined in
`gdiff-program-options' are also passed to `gdiff-program'."
  (with-current-buffer (generate-new-buffer "gdiff")
    (setq gdiff-cleanup-function cleanup
          f1 (gdiff-file-name f1)
          f2 (gdiff-file-name f2))
    (message "gdiff start %s %s %s %s"
             gdiff-program
             (mapconcat 'identity gdiff-program-options " ")
             f1 f2)
    (let ((pid (apply 'start-process (buffer-name) (current-buffer)
                      gdiff-program
                      (append gdiff-program-options (list f1 f2)))))
      (set-process-sentinel pid #'gdiff-process-sentinel)
      (message "gdiff process '%s' -- %S"
               (process-name pid) (process-status pid)))))

(defun gdiff-process-sentinel (process event)
  "Handle gdiff PROCESS EVENT.
At process end call the `gdiff-cleanup-function' if defined."
  (let ((pname   (process-name   process))
        (pbuffer (process-buffer process))
        (pstatus (process-status process)))
    (message "gdiff process '%s' -- %S" pname pstatus)
    (if (or (eq pstatus 'exit) (eq pstatus 'signal))
        (if (buffer-name pbuffer)
            (with-current-buffer pbuffer
              (and (functionp gdiff-cleanup-function)
                   (funcall gdiff-cleanup-function))
              (kill-buffer pbuffer))
          (message "gdiff process '%s' buffer killed" pname)))))

(defun gdiff-buffer-file-name ()
  "Return current buffer file name.
Make sure the current buffer and its working file are in sync and
workfile changed since last checkout."
  (vc-buffer-sync)
  (if (vc-workfile-unchanged-p buffer-file-name)
      (error "No changes to %s since latest version" buffer-file-name))
  (buffer-file-name))

(defun gdiff-delete-file (f)
  "Delete file F and display a status message."
  (delete-file f)
  (message "File '%s' %sdeleted" f (if (file-exists-p f) "not " "")))
        
(defun gdiff-file-name (f)
  "Return a native OS filename from the given Emacs filename F.
For now, if on MS-DOS like OS, replace occurences of `/' by '\\'.
Remove trailing `/'."
  (let ((f (expand-file-name f)))
    (if (eq (aref f (1- (length f))) ?/)
        (setq f (substring f 0 -1)))
    (if (memq system-type '(ms-dos windows-nt))
        (setq f (gdiff-subst-char-in-string ?/ ?\\ f)))
    f))

;;;###autoload
(defun gdiff-buffer-with-file (&optional buffer)
  "View the differences between BUFFER and its associated file."
  (interactive "bBuffer: ")
  (setq buffer (get-buffer (or buffer (current-buffer))))
  (let* ((basefile
          (or (buffer-file-name buffer)
              (error "Buffer %s has no associated file" buffer)))
         (tempfile (make-temp-file "buffer-content-")))
    (with-current-buffer buffer
      (save-restriction
        (widen)
        (write-region (point-min) (point-max) tempfile nil 'silent)))
    (gdiff-execute-nowait basefile tempfile
                          `(lambda ()   ; cleanup temp working files
                             (gdiff-delete-file ,tempfile)))
    nil))

;;;###autoload
(defun gdiff-files (file1 file2)
  "Compare FILE1 with FILE2."
  (interactive "fCompare file: \nfwith file: ")
  (gdiff-execute-nowait file1 file2))

;;;###autoload
(defun gdiff-folders (folder1 folder2)
  "Compare FOLDER1 with FOLDER2."
  (interactive "DCompare folder: \nDwith folder: ")
  (gdiff-execute-nowait folder1 folder2))

;;;###autoload
(defun gdiff-latest-revision (&optional file)
  "Compare FILE with its most recent checked in version.
Optional argument FILE default to the file visited by the current
buffer."
  ;; if buffer is non-nil, use that buffer instead of the current buffer
  (interactive "P")
  (if (stringp file) (find-file file))
  (gdiff-vc-internal "" ""))

;;;###autoload
(defun gdiff-revision (&optional file)
  "Compare two revisions of FILE.
Optional argument FILE default to the file visited by the current
buffer."
  ;; if buffer is non-nil, use that buffer instead of the current buffer
  (interactive "P")
  (if (stringp file) (find-file file))
  (let* ((what (if (stringp file)
                   (file-name-nondirectory file) "current buffer"))
         (rev1 (read-string
                (format "Base revision (default: %s's latest one): " what)))
         (rev2 (read-string
                (format "Compared revision (default: %s): " what))))
    (gdiff-vc-internal rev1 rev2)))

(provide 'gdiff)
(run-hooks 'gdiff-load-hook)

;;; gdiff.el ends here
