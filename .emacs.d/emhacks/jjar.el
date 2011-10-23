;;; jjar.el --- Java Archive builder

;; Copyright (C) 1999, 2000, 2003, 2005 by David Ponce

;; Author: David Ponce david@dponce.com
;; Maintainer: David Ponce david@dponce.com
;; Created: June 14 1999
;; Keywords: tools
;; Revision: $Id: jjar.el,v 1.16 2005/07/05 07:00:36 ponced Exp $

(defconst jjar-version "1.9")

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
;; This library provides commands to create or update (when supported)
;; a Java ARrchive files (jar). The files to be archived are searched
;; in a given directory tree in this way:
;;
;; 1. A dialog is shown to customize shell wildcard expressions used
;;    to locate the files.
;; 2. A second dialog permits to select the files to archive, in those
;;    found using the above wildcard expressions.
;; 3. The jar command is built and run in compile-mode.
;;
;; M-x `jjar-create' create a new jar file.
;; M-x `jjar-update' update an existing jar file (if supported).
;;
;; Customize the `jjar-jar-command' option to specify where to find
;; the jar command on your installation.
;;
;; To install and use, put this file on your Emacs-Lisp load path and
;; add the following into your ~/.emacs startup file:
;;
;; (autoload 'jjar-create "jjar" "Create a new jar file."       t)
;; (autoload 'jjar-update "jjar" "Update an existing jar file." t)
;;

;;; History:
;;

;;; Code:
(require 'compile)
(require 'wid-edit)
(eval-when-compile (require 'cl))

;;; Compatibility
;;
(if (fboundp 'overlay-lists)
    (defalias 'jjar-overlay-lists
      'overlay-lists)
  (defalias 'jjar-overlay-lists
    '(lambda () (list (extent-list)))))

(if (fboundp 'delete-overlay)
    (defalias 'jjar-delete-overlay
      'delete-overlay)
  (defalias 'jjar-delete-overlay
    'delete-extent))

;;; Options
;;
(defgroup jjar nil
  "Java Archive builder."
  :group 'tools
  :prefix "jjar-")

(defcustom jjar-jar-command "jar"
  "*Path to the jar command."
  :group 'jjar
  :type 'string)

(defcustom jjar-jar-nocompress-option t
  "*If non-nil, jar don't use ZIP compression."
  :group 'jjar
  :type 'boolean)

(defcustom jjar-jar-verbose-option t
  "*If non-nil, jar generates verbose output on standard error."
  :group 'jjar
  :type 'boolean)

(defcustom jjar-include-wildcards '("*.class" "*.properties")
  "*List of predefined shell wildcard expressions used to locate files.
The default setting includes class and properties files."
  :group 'jjar
  :type '(repeat regexp))

(defcustom jjar-load-hook nil
  "Hook run when package has been loaded."
  :group 'jjar
  :type 'hook)

;;; Internals
;;
(defvar jjar-build-option nil
  "The build option used by the jar command.
That is \"-c\" to create a new jar file, or \"-u\" to update an
existing one.")

(defvar jjar-base-directory nil
  "Directory where the jar command is running.")

(defvar jjar-jar-file nil
  "Name of the jar file to be created or updated.")

(defvar jjar-wildcards nil
  "List of shell wildcards used to locate the files to be archived.")

(defvar jjar-files nil
  "List of file expressions passed to the jar command.")

(defun jjar-get-matching-files ()
  "Return a list of file expressions to pass to the jar command.
That is the shell wildcard expressions in `jjar-wildcards' that match
files in `jjar-base-directory' directory tree."
  (message "Searching matching files...")
  (let ((file-list
         (nconc (and (file-directory-p jjar-base-directory)
                     (jjar-get-matching-files-in-dir jjar-base-directory))
                (jjar-get-matching-files-in-tree jjar-base-directory))))
    (message "Searching matching files...Done")
    file-list))

(defun jjar-get-matching-files-in-dir (dir)
  "Return a list of wildcard expressions matching files in DIR.
Wildcard expressions are in variable `jjar-wildcards'."
  (let ((dir (file-name-as-directory dir))
        (wildcards jjar-wildcards)
        wildcard dir-wc file-wc files file match wl)
    (while wildcards
      (setq wildcard  (car wildcards)
            file-wc   (or (file-name-nondirectory wildcard) "*")
            dir-wc    (or (file-name-directory wildcard) "*")
            wildcards (cdr wildcards))
      (and (string-match (wildcard-to-regexp (concat dir-wc "*")) dir)
           (directory-files dir t (wildcard-to-regexp file-wc))
           (setq wl (cons
                     (concat
                      (file-name-as-directory
                       (file-relative-name dir jjar-base-directory))
                      file-wc)
                     wl))))
    (nreverse wl)))

(defun jjar-get-matching-files-in-tree (dir)
  "Return wildcard expressions matching files in DIR tree.
DIR is a subdirectory in `jjar-base-directory' tree."
  (apply 'nconc
         (mapcar #'(lambda (f)
                     (and (not (string-match "\\.\\'" f))
                          (file-directory-p f)
                          (nconc (jjar-get-matching-files-in-dir  f)
                                 (jjar-get-matching-files-in-tree f))))
                 (directory-files dir t))))

(defun jjar-make-jar-command ()
  "Build and return a jar command ready to run."
  (and jjar-files
       (concat jjar-jar-command " " 
               (jjar-get-jar-options) " "
               jjar-jar-file " "
               (mapconcat 'identity jjar-files " "))))

(defun jjar-get-jar-options ()
  "Build and return the jar command options."
  (let ((options jjar-build-option))
    (if jjar-jar-nocompress-option
        (setq options (concat options "0")))
    (if jjar-jar-verbose-option
        (setq options (concat options "v")))
    (setq options (concat options "f"))
    options))

(defun jjar-execute ()
  "Build and run the jar command in compile mode."
  (let ((compile-command (jjar-make-jar-command)))
    (when compile-command
      (save-some-buffers (not compilation-ask-about-save) nil)
      (let ((default-directory jjar-base-directory))
        (compile-internal compile-command "No more errors")))))

;;; Dialogs
;;
(defun jjar-cancel-dialog (&rest ignore)
  "Cancel the current dialog.
IGNORE arguments."
  (interactive)
  (kill-buffer (current-buffer))
  (error "Dialog canceled"))

(defvar jjar-dialog-mode-map
  (let ((km (make-sparse-keymap)))
    (define-key km "q" 'jjar-cancel-dialog)
    (define-key km [down-mouse-1] 'widget-button-click)
    (set-keymap-parent km widget-keymap)
    km)
  "Keymap used in jjar dialogs.")

(defun jjar-dialog-mode ()
  "Major mode used in jjar dialogs.

\\{jjar-dialog-mode-map}."
  (interactive)
  (setq major-mode 'jjar-dialog-mode)
  (setq mode-name "jjar-dialog")
  (use-local-map jjar-dialog-mode-map))

(defun jjar-wildcards-dialog (wildcards)
  "Dialog to customize shell wildcard expressions that locate files.
WILDCARDS is a list of wildcard expressions to initialize the dialog."
  (with-current-buffer (get-buffer-create "*jjar-wildcards-dialog*" )
    (switch-to-buffer (current-buffer))
    ;; Cleanup buffer
    (kill-all-local-variables)
    (let ((inhibit-read-only t)
          (ol (jjar-overlay-lists)))
      (erase-buffer)
      ;; Delete all the overlays.
      (mapc 'jjar-delete-overlay (car ol))
      (mapc 'jjar-delete-overlay (cdr ol)))
    ;; Initialize the wildcards with the predefined ones.
    (setq jjar-wildcards wildcards)
    ;; Insert the dialog header.
    (widget-insert "\
Customize the wildcard expressions used to locate files.\n\n")
    ;; Insert an editable list of the wildcard expressions.
    (widget-create
     'editable-list
     :entry-format "%i %d %v"
     :notify (lambda (widget &rest ignore)
               (setq jjar-wildcards (widget-value widget))
               ;;(message "%S" jjar-wildcards)
               )
     :value jjar-wildcards
     '(editable-field :value ""))
    
    ;; Insert the Next button
    (widget-insert "\n\n")
    (widget-create
     'push-button
     :notify (lambda (&rest ignore)
               (if (not jjar-wildcards)
                   (message "No wildcard expression selected.")
                 (kill-buffer (current-buffer))
                 (jjar-files-dialog)))
     "Next")

    ;; Insert the cancel button
    (widget-insert " ")
    (widget-create
     'push-button
     :notify 'jjar-cancel-dialog
     "Cancel")
    (jjar-dialog-mode)
    (widget-setup)
    (goto-char (point-min))))

(defun jjar-files-dialog-toggle-selection (widget &rest ignore)
  "Checkbox WIDGET action that toogles a directory selection.
Used internally by `jjar-files-dialog'.
IGNORE other arguments."
(let ((value (widget-get widget ':tag)))
    ;; if value is already in the selected items
    (if (memq value jjar-files)
        ;; then remove it
        (progn
          (setq jjar-files (delq value jjar-files))
          (message "%s removed from selection" value))
      ;; else add it
      (push value jjar-files)
      (message "%s added to selection" value))))

(defun jjar-files-dialog ()
  "Dialog to select file expressions passed to the jar command."
  (with-current-buffer (get-buffer-create "*jjar-files-dialog*")
    (switch-to-buffer (current-buffer))
    ;; Cleanup buffer
    (kill-all-local-variables)
    (let ((inhibit-read-only t)
          (ol (jjar-overlay-lists)))
      (erase-buffer)
      ;; Delete all the overlays.
      (mapc 'jjar-delete-overlay (car ol))
      (mapc 'jjar-delete-overlay (cdr ol)))
    ;; Initialize the list of files.
    (setq jjar-files (jjar-get-matching-files))
    ;; Insert the dialog header.
    (widget-insert
     (format "Select the files in '%s' to archive.\n\n"
             jjar-base-directory))
    ;; Insert the list of sub-directories as checkboxes
    (dolist (f jjar-files)
      (widget-create
       'checkbox
       :value (memq f jjar-files)
       :format "\n %[%v%]  %t"
       :tag f
       :notify 'jjar-files-dialog-toggle-selection))
    ;; Insert the Back button.
    (widget-insert "\n\n")
    (widget-create
     'push-button
     :notify (lambda (&rest ignore)
               (kill-buffer (current-buffer))
               (jjar-wildcards-dialog jjar-wildcards))
     "Back")
    ;; Insert the Next button.
    (widget-insert " ")
    (widget-create
     'push-button
     :notify (lambda (&rest ignore)
               (if (not jjar-files)
                   (message "No file selected")  
                 (kill-buffer (current-buffer))
                 (jjar-execute)))
     "Next")
    ;; Insert the Cancel button
    (widget-insert " ")
    (widget-create
     'push-button
     :notify 'jjar-cancel-dialog
     "Cancel")
    (jjar-dialog-mode)
    (widget-setup)
    (goto-char (point-min))))

;;; Commands
;;

;;;###autoload
(defun jjar-create (base)
  "Create a new jar file.
BASE is the directory where to run the jar command, that is where the
files to be archived are searched."
  (interactive "DBase-directory: ")
  (setq jjar-build-option "-c"
        jjar-base-directory (file-name-as-directory
                             (expand-file-name base))
        jjar-jar-file (file-relative-name
                       (read-file-name "jar-file: "
                                       jjar-base-directory)
                       jjar-base-directory))
  (jjar-wildcards-dialog jjar-include-wildcards))

;;;###autoload
(defun jjar-update (base file)
  "Update an existing jar file.
BASE is the directory where to run the jar command, that is where the
files to be archived are searched.
FILE is the name of the jar file to update."
  (interactive "DBase-directory: \nfJAR-file: ")
  (setq jjar-build-option "-u"
        jjar-base-directory (file-name-as-directory
                             (expand-file-name base))
        jjar-jar-file (file-relative-name file jjar-base-directory))
  (jjar-wildcards-dialog jjar-include-wildcards))

(provide 'jjar)

(run-hooks 'jjar-load-hook)

;;; jjar.el ends here
