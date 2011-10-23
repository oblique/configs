;;; gdiff-setup.el -- gdiff setup

;; Copyright (C) 2002, 2003, 2005 by David Ponce

;; Author: David Ponce <david@dponce.com>
;; Maintainer: David Ponce <david@dponce.com>
;; Created: March 15 2000
;; Keywords: tools
;; Revision: $Id: gdiff-setup.el,v 1.4 2005/07/05 07:00:36 ponced Exp $

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
;; This is the always-loaded portion of gdiff that setups shortcut
;; keys, menu and autoloads.
;;

;;; History::
;;

;;; Code:
(require 'vc)

;;; Compatibility
(or (fboundp 'vc-ensure-vc-buffer)
    
    ;; Copied from GNU Emacs vc.el
    (defun vc-ensure-vc-buffer ()
      "Make sure that the current buffer visits a version-controlled file."
      (if vc-dired-mode
          (set-buffer (find-file-noselect (dired-get-filename)))
        (while vc-parent-buffer
          (pop-to-buffer vc-parent-buffer))
        (if (not buffer-file-name)
            (error "Buffer %s is not associated with a file" (buffer-name))
          (if (not (vc-backend buffer-file-name))
              (error "File %s is not under version control" buffer-file-name)))))
    
    )

;;; Options
;;
(defgroup gdiff nil
  "Use a GUI diff tool from Emacs."
  :group 'tools)

(defcustom gdiff-program nil
  "*Path to the external GUI diff tool."
  :group 'gdiff
  :type 'file)

(defcustom gdiff-program-options nil
  "*Options to pass to the external GUI diff tool."
  :group 'gdiff
  :type '(repeat string))

(defcustom gdiff-load-hook nil
  "*Hook run when package has been loaded."
  :group 'gdiff
  :type 'hook)

(defvar gdiff-menu
  (list "Compare (GUI)"
        ["Buffer Changes"
         gdiff-buffer-with-file
         (buffer-file-name)]
        ["Two Files..."
         gdiff-files
         t]
        ["Two Folders..."
         gdiff-folders
         t]
        ["File with Revision..."
         gdiff-revision
         (gdiff-check-if-vc-buffer)]
        ["With Last Version"
         gdiff-latest-revision
         (gdiff-check-if-vc-buffer)]
        )
  "The gdiff menu.")

(defvar gdiff-prefix-map nil
  "The gdiff prefix keymap.")

;;;### (autoloads (gdiff-revision gdiff-latest-revision gdiff-folders
;;;;;;  gdiff-files gdiff-buffer-with-file) "gdiff" "gdiff.el" (15715
;;;;;;  37770))
;;; Generated autoloads from gdiff.el

(autoload (quote gdiff-buffer-with-file) "gdiff" "\
View the differences between BUFFER and its associated file." t nil)

(autoload (quote gdiff-files) "gdiff" "\
Compare FILE1 with FILE2." t nil)

(autoload (quote gdiff-folders) "gdiff" "\
Compare FOLDER1 with FOLDER2." t nil)

(autoload (quote gdiff-latest-revision) "gdiff" "\
Compare FILE with its most recent checked in version.
Optional argument FILE default to the file visited by the current
buffer." t nil)

(autoload (quote gdiff-revision) "gdiff" "\
Compare two revisions of FILE.
Optional argument FILE default to the file visited by the current
buffer." t nil)

;;;***

;;; Menu & Keymap
;;
(defun gdiff-check-if-vc-buffer ()
  "Return t if the current buffer visits a version-controlled file."
  (condition-case nil
      (progn
        (vc-ensure-vc-buffer)
        t)
    (error nil)))

;; Add the gdiff menu before the Tools/Compare menu
(if (and (condition-case nil
             (require 'easymenu "easymenu")
           (error nil))
         (fboundp 'easy-menu-add-item))
    (easy-menu-add-item nil '("tools") gdiff-menu "compare"))

;; Setup gdiff keys
(setq gdiff-prefix-map (lookup-key global-map "\C-c="))

(if (keymapp gdiff-prefix-map)
    nil
  (setq gdiff-prefix-map (make-sparse-keymap))
  (define-key global-map "\C-c=" gdiff-prefix-map)
  (define-key gdiff-prefix-map "b" 'gdiff-buffer-with-file)
  (define-key gdiff-prefix-map "f" 'gdiff-files)
  (define-key gdiff-prefix-map "d" 'gdiff-folders)
  (define-key gdiff-prefix-map "r" 'gdiff-revision)
  (define-key gdiff-prefix-map "=" 'gdiff-latest-revision))

(provide 'gdiff-setup)

;;; gdiff-setup.el ends here
