;;; tree-widget-examples.el --- basic examples using the tree-widget

;; Copyright (C) 2001, 2003, 2005 by David Ponce

;; Author: David Ponce <david@dponce.com>
;; Maintainer: David Ponce <david@dponce.com>
;; Created: 27 Nov 2001
;; Keywords: extensions
;; Revision: $Id: tree-widget-examples.el,v 1.12 2006/05/29 13:14:56 ponced Exp $

(defconst tree-widget-examples-version "1.3")

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
;; This library gives simple examples of use of the `tree-widget'.
;;

;;; History:
;;

;;; Code:
;;
(require 'tree-widget)

;;; Compatibility
;;
(defalias 'tree-widget-example-overlay-lists
  (if (fboundp 'overlay-lists)
      'overlay-lists
    '(lambda () (list (extent-list)))))

(defalias 'tree-widget-example-delete-overlay
  (if (fboundp 'delete-overlay)
      'delete-overlay
    'delete-extent))

;;; Common stuff.
;;
(defmacro tree-widget-example-dialog (title &rest forms)
  "Show a dialog buffer named TITLE, setup with FORMS."
  (declare (indent 1) (debug t))
  `(with-current-buffer (get-buffer-create ,title)
     ;; Cleanup buffer
     (let ((inhibit-read-only t)
           (ol (tree-widget-example-overlay-lists)))
       (dolist (o (car ol))
         (tree-widget-example-delete-overlay o))
       (dolist (o (cdr ol))
         (tree-widget-example-delete-overlay o))
       (erase-buffer))
     (kill-all-local-variables)
     (widget-insert (format "%s.\n\n" (buffer-name)))
     ,@forms
     ;; Insert a Close button.
     (widget-insert "\n")
     (widget-create
      'push-button
      :button-keymap tree-widget-button-keymap ; XEmacs
      :keymap        tree-widget-button-keymap ; Emacs
      :notify 'tree-widget-example-close
      "Close")
     (use-local-map widget-keymap)
     (widget-setup)
     (goto-char (point-min))
     (widget-move 1)
     (switch-to-buffer (current-buffer))))

(defun tree-widget-example-close (&rest ignore)
  "Close the current dialog.
IGNORE arguments."
  (interactive)
  (kill-buffer (current-buffer)))

;;; Example 1
;;
(defun tree-widget-example-1 (&optional theme)
  "A simple usage of the `tree-widget'.
Optional argument THEME is an image theme to use to draw the tree.  It
default to the global theme defined in option `tree-widget-theme'.
To be prompted for a theme, use
 \\[universal-argument] \\[tree-widget-example-1]."
  (interactive
   (list (if current-prefix-arg
             (completing-read "Theme name: "
                              '(("default" . "default")
                                ("folder"  . "folder")))
           nil)))
  (tree-widget-example-dialog "*`tree-widget' example 1*"
    (tree-widget-set-theme theme)
    (apply
     'widget-create
     ;; Open this level.
     '(tree-widget
       :open t
       ;; Use a push button for this node.
       :node (push-button
              :tag "Root"
              :format "%[%t%]\n"
              :notify
              (lambda (&rest ignore)
                (message "This is the Root node")))
       ;; Add subtrees (their nodes defaut to items).
       (tree-widget :tag "Node-1 (empty)")
       (tree-widget :tag "Node-2"
                    (tree-widget :tag "Node-2.1 (empty)")
                    (tree-widget :tag "Node-2.2"
                                 (item :tag "Leaf-2.2.1")
                                 (item :tag "Leaf-2.2.2")))
       (tree-widget :tag "Node-3"
                    (tree-widget :tag "Node-3.1 (empty)")
                    (item        :tag "Leaf-3.2"))))
    ))

;;; Example 2
;;
(defvar tree-widget-example-2-open-nodes nil
  "List of names of opened nodes.
Initially all tree nodes are closed.")

(defun tree-widget-example-2-open-p (node-name)
  "Return non-nil if NODE-NAME is the name of an open tree node."
  (member node-name tree-widget-example-2-open-nodes))

(defun tree-widget-example-2-track-open-state (tree)
  "Track the open status of the tree widget TREE.
Maintain a persistent list of opened nodes, to recover the open state
across invocations of the `tree-widget-example-2' command."
  (let ((node-name (widget-get tree :node-name)))
    (if (widget-get tree :open)
        (add-to-list 'tree-widget-example-2-open-nodes node-name)
      (setq tree-widget-example-2-open-nodes
            (delete node-name tree-widget-example-2-open-nodes)))))

(defun tree-widget-example-2 ()
  "A simple usage of the `tree-widget'.
Each node open/close state is persistent across invocations of the
command."
  (interactive)
  (tree-widget-example-dialog "*`tree-widget' example 2*"
    ;; Setup the hook that tracks the open status of nodes.
    (make-local-hook 'tree-widget-after-toggle-functions)
    (add-hook 'tree-widget-after-toggle-functions
              'tree-widget-example-2-track-open-state nil t)
    ;; Create the tree.
    (apply
     'widget-create
     ;; Open this level.
     `(tree-widget
       :node-name "0" ;; Node unique ID
       :open ,(tree-widget-example-2-open-p "0")
       ;; Use a push button for this node.
       :node (push-button :tag "Root" :format "%[%t%]\n"
                          :notify (lambda (&rest ignore)
                                    (message "This is the Root node")))
       ;; Add subtrees (their nodes defaut to items).
       (tree-widget
        :tag "Child-1"
        :node-name "1"
        :open ,(tree-widget-example-2-open-p "1"))
       (tree-widget
        :tag "Child-2"
        :node-name "2"
        :open ,(tree-widget-example-2-open-p "2")
        (tree-widget
         :tag "Child-2.1"
         :node-name "2.1"
         :open ,(tree-widget-example-2-open-p "2.1"))
        (tree-widget
         :tag "Child-2.2"
         :node-name "2.2"
         :open ,(tree-widget-example-2-open-p "2.2")
         (tree-widget
          :tag "Child-2.2.1"
          :node-name "2.2.1"
          :open ,(tree-widget-example-2-open-p "2.2.1"))
         (tree-widget
          :tag "Child-2.2.2"
          :node-name "2.2.2"
          :open ,(tree-widget-example-2-open-p "2.2.2"))))
       (tree-widget
        :tag "Child-3"
        :node-name "3"
        :open ,(tree-widget-example-2-open-p "3")
        (tree-widget
         :tag "Child-3.1"
         :node-name "3.1"
         :open ,(tree-widget-example-2-open-p "3.1"))
        (tree-widget
         :tag "Child-3.2"
         :node-name "3.2"
         :open ,(tree-widget-example-2-open-p "3.2")))))
    ))

;;; Example 3
;;
(defun tree-widget-example-3-expander (widget)
  "Return the widgets children of WIDGET tree widget."
  (message "tree-widget-example-3-expander...")
  (prog1
      '((tree-widget :tag "Node-2.1 (empty)")
        (tree-widget :tag "Node-2.2"
                     (item :tag "Leaf-2.2.1")
                     (item :tag "Leaf-2.2.2")))
    (message "tree-widget-example-3-expander...done")))

(defun tree-widget-example-3 ()
  "A simple usage of the `tree-widget' with dynamic expansion."
  (interactive)
  (tree-widget-example-dialog "*`tree-widget' example 2*"
    (apply
     'widget-create
     ;; Open this level.
     '(tree-widget
       :open t
       ;; Use a push button for this node.
       :node (push-button
              :tag "Root" :format "%[%t%]\n"
              :notify (lambda (&rest ignore)
                        (message "This is the Root node")))
       ;; Add subtrees (their nodes defaut to items).
       (tree-widget
        :tag "Node-1 (empty)")
       ;; Dynamically retrieve children of this node.
       (tree-widget
        :tag "Node-2 (dynamic)"
        :expander tree-widget-example-3-expander)
       (tree-widget
        :tag "Node-3"
        (tree-widget
         :tag "Node-3.1 (empty)")
        (item
         :tag "Leaf-3.2"))))
    ))

;;; Example 4
;;
(define-widget 'tree-widget-example-4-open-icon
  'tree-widget-open-icon
  ""
  :tag "<<")
(define-widget 'tree-widget-example-4-close-icon
  'tree-widget-close-icon
  ""
  :tag ">>")
(define-widget 'tree-widget-example-4-empty-icon
  'tree-widget-empty-icon
  ""
  :tag "<>")
(define-widget 'tree-widget-example-4-leaf-icon
  'tree-widget-leaf-icon
  ""
  :tag " `")

;; Old compatible guides schema
(define-widget 'tree-widget-example-4-guide
  'tree-widget-guide
  ""
  :tag " ")
(define-widget 'tree-widget-example-4-end-guide
  'tree-widget-end-guide
  ""
  :tag " ")
(define-widget 'tree-widget-example-4-no-guide
  'tree-widget-no-guide
  ""
  :tag " ")
(define-widget 'tree-widget-example-4-handle
  'tree-widget-handle
  ""
  :tag "")
(define-widget 'tree-widget-example-4-no-handle
  'tree-widget-no-handle
  ""
  :tag "")

(define-widget 'tree-widget-example-4-old-tree 'tree-widget
  "Tree widget with a customized ASCII look and feel."
  :open-icon  'tree-widget-example-4-open-icon
  :close-icon 'tree-widget-example-4-close-icon
  :empty-icon 'tree-widget-example-4-empty-icon
  :leaf-icon  'tree-widget-example-4-leaf-icon
  :guide      'tree-widget-example-4-guide
  :end-guide  'tree-widget-example-4-end-guide
  :no-guide   'tree-widget-example-4-no-guide
  :handle     'tree-widget-example-4-handle
  :no-handle  'tree-widget-example-4-no-handle
  :guides     'tree-widget-old-guides
  )

;; New guides schema
(define-widget 'tree-widget-example-4-mid-guide
  'tree-widget-mid-guide
  ""
  :tag " ")
(define-widget 'tree-widget-example-4-skip-guide
  'tree-widget-skip-guide
  ""
  :tag "")
(define-widget 'tree-widget-example-4-node-guide
  'tree-widget-node-guide
  ""
  :tag " ")
(define-widget 'tree-widget-example-4-last-node-guide
  'tree-widget-last-node-guide
  ""
  :tag " ")

(define-widget 'tree-widget-example-4-guides 'tree-widget-default-guides
  "Example 4 default guides."
  :mid-guide       'tree-widget-example-4-mid-guide
  :skip-guide      'tree-widget-example-4-skip-guide
  :node-guide      'tree-widget-example-4-node-guide
  :last-node-guide 'tree-widget-example-4-last-node-guide
  )

(define-widget 'tree-widget-example-4-tree 'tree-widget
  "Tree widget with a customized ASCII look and feel."
  :open-icon  'tree-widget-example-4-open-icon
  :close-icon 'tree-widget-example-4-close-icon
  :empty-icon 'tree-widget-example-4-empty-icon
  :leaf-icon  'tree-widget-example-4-leaf-icon
  :guides 'tree-widget-example-4-guides
  )

(defun tree-widget-example-4 (&optional arg)
  "A tree widget with a customized ASCII look and feel."
  (interactive "P")
  (let ((tree-widget (if arg 'tree-widget-example-4-old-tree
                       'tree-widget-example-4-tree)))
    (tree-widget-example-dialog "*`tree-widget' example 4*"
      ;; Don't use images.
      (set (make-local-variable 'tree-widget-image-enable) nil)

      ;; Create the tree.  Fully expanded it will look like this:
      ;;
      ;;   << 1
      ;;    << 1.0
      ;;     << 1.0.1
      ;;       ` 1.0.1.1
      ;;       ` 1.0.1.2
      ;;      << 1.0.1.3
      ;;        ` 1.0.1.3.1
      ;;        ` 1.0.1.3.2
      ;;     << 1.0.2
      ;;       ` 1.0.2.1
      ;;      << 1.0.2.2
      ;;        ` 1.0.2.2.1
      ;;    <> 1.1
      ;;    << 1.2
      ;;      ` 1.2.1
      ;;      ` 1.2.2
      (apply
       'widget-create
       `(,tree-widget
         :tag "1" :open t :indent 2
         (,tree-widget
          :tag "1.0"
          (,tree-widget
           :tag "1.0.1"
           (item :tag "1.0.1.1")
           (item :tag "1.0.1.2")
           (,tree-widget
            :tag "1.0.1.3"
            (item :tag "1.0.1.3.1")
            (item :tag "1.0.1.3.2")
            ))
          (,tree-widget
           :tag "1.0.2"
           (item :tag "1.0.2.1")
           (,tree-widget
            :tag "1.0.2.2"
            (item :tag "1.0.2.2.1")
            )))
         (,tree-widget
          :tag "1.1")
         (,tree-widget
          :tag "1.2" :open t
          (item :tag "1.2.1")
          (item :tag "1.2.2")))))))

(provide 'tree-widget-examples)

;;; tree-widget-examples.el ends here
