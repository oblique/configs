;;; dir-tree.el --- Sophisticated example of `tree-widget' usage

;; Copyright (C) 2001, 2003, 2005 by David Ponce

;; Author: David Ponce <david@dponce.com>
;; Maintainer: David Ponce <david@dponce.com>
;; Created: 16 Feb 2001
;; Keywords: extensions
;; Revision: $Id: dir-tree.el,v 1.9 2005/07/05 07:00:36 ponced Exp $

(defconst dir-tree-version "1.2")

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

;;; Commentary:
;;
;; This library is a sophisticated example of usage of the tree-widget
;; library.  It provides the `dir-tree' command which shows the
;; content of a directory in a tree widget.
;;
;; A directory content is read when the corresponding tree node is
;; expanded for the first time.  To refresh the content of an expanded
;; directory, click on its name with the mouse-2 button.  Also, you
;; can change the sample select/unselect state of a file by clicking
;; on its name with the mouse-2 button.
;;
;; To install and use, put this file on your Emacs-Lisp load path and
;; add the following into your ~/.emacs start up file:
;;
;; (require 'dir-tree)
;;

;;; History:
;;

;;; Code:
(eval-when-compile (require 'cl))
(require 'tree-widget)

;;; Compatibility
;;
(defalias 'dir-tree-overlay-lists
  (if (fboundp 'overlay-lists)
      'overlay-lists
    '(lambda () (list (extent-list)))))

(defalias 'dir-tree-delete-overlay
  (if (fboundp 'delete-overlay)
      'delete-overlay
    'delete-extent))

;;; Widgets
;;
(define-widget 'dir-tree-dir-widget 'tree-widget
  "Directory Tree widget."
  :expander      'dir-tree-expand-dir
  :indent        4)

(define-widget 'dir-tree-file-widget 'push-button
  "File widget."
  :doc           "*"
  ;; When a directory node is collapsed, keep the :doc value of file
  ;; widgets, which hold their sample selected/unselected state.
  :keep          '(:doc)
  :format        "%[%t%]%d"
  :button-face   'default
  :notify        'dir-tree-toggle-selection
  :help-echo     "Click to toggle file selection"
  :button-keymap tree-widget-button-keymap  ; XEmacs
  :keymap        tree-widget-button-keymap  ; Emacs
  )

;;; Callbacks
;;
(defun dir-tree-toggle-selection (widget &rest ignore)
  "Change the selected state of WIDGET.
IGNORE other arguments."
  (if (string-equal (widget-get widget :doc) "*")
      (widget-put widget :doc "-")
    (widget-put widget :doc "*"))
  ;; Redraw the tree node.
  (widget-value-set widget (widget-value widget))
  (message "File %s %sselected"
           (widget-get widget :path)
           (if (widget-get widget :tag ) "un" "")))

(defun dir-tree-refresh-dir (widget &rest ignore)
  "Refresh the tree widget, parent of WIDGET.
IGNORE other arguments."
  (let ((tree (widget-get widget :parent)))
    ;; Clear children.
    (widget-put tree :args nil)
    ;; Redraw the tree node.
    (widget-value-set tree (widget-value tree))))

(defun dir-tree-widget (e)
  "Return a widget to display file or directory E."
  (if (file-directory-p e)
      `(dir-tree-dir-widget
        :path ,e
        :node (push-button
               :tag ,(file-name-as-directory
                      (file-name-nondirectory e))
               :format "%[%t%]\n"
               :notify dir-tree-refresh-dir
               :help-echo "Click to refresh directory content"
               :button-keymap ,tree-widget-button-keymap  ; XEmacs
               :keymap        ,tree-widget-button-keymap  ; Emacs
               ))
    `(dir-tree-file-widget
      :path ,e
      :tag  ,(file-name-nondirectory e))))

(defun dir-tree-list (dir)
  "Return the content of the directory DIR.
Return the list of components found, with sub-directories at the
beginning of the list."
  (let (files dirs)
    (dolist (entry (directory-files dir 'full))
      (unless (string-equal (substring entry -1) ".")
        (if (file-directory-p entry)
            (push entry dirs)
          (push entry files))))
    (nreverse (nconc files dirs))))

(defun dir-tree-expand-dir (tree)
  "Expand the tree widget TREE.
Return a list of child widgets."
  (let ((dir (widget-get tree :path)))
    (message "Reading directory %s..." dir)
    (condition-case err
        (prog1
            (mapcar 'dir-tree-widget (dir-tree-list dir))
          (message "Reading directory %s...done" dir))
      (error
       (message "%s" (error-message-string err))
       nil))))

;;; Command
;;
(defun dir-tree-close (&rest ignore)
  "Close the current dialog.
IGNORE arguments."
  (interactive)
  (kill-buffer (current-buffer)))

(defun dir-tree (root)
  "Display the ROOT directory in a tree widget."
  (interactive "DRoot: ")
  (with-current-buffer
      (get-buffer-create (format "* %s directory tree*" root))
    ;; Cleanup buffer
    (let ((inhibit-read-only t)
          (ol (dir-tree-overlay-lists)))
      (dolist (o (car ol))
        (dir-tree-delete-overlay o))
      (dolist (o (cdr ol))
        (dir-tree-delete-overlay o))
      (erase-buffer))
    (kill-all-local-variables)
    ;; Build the tree.
    (widget-insert (format "%s directory tree. \n\n" root))
    (tree-widget-set-theme "folder")
    (let ((default-directory root))
      (widget-create (dir-tree-widget root)))
    ;; Insert a Close button.
    (widget-insert "\n")
    (widget-create
     'push-button
     :button-keymap tree-widget-button-keymap  ; XEmacs
     :keymap        tree-widget-button-keymap  ; Emacs
     :notify 'dir-tree-close
     "Close")
    (use-local-map widget-keymap)
    (widget-setup)
    (goto-char (point-min))
    (switch-to-buffer (current-buffer))))

(provide 'dir-tree)

;;; dir-tree.el ends here
