;;; overlay-fix.el --- overlay bug workaround

;; Copyright (C) 2001, 2003, 2005 by David Ponce

;; Author: David Ponce <david@dponce.com>
;; Maintainer: David Ponce <david@dponce.com>
;; Created: 12 Feb 2001
;; Revision: $Id: overlay-fix.el,v 1.4 2005/07/05 07:00:37 ponced Exp $

(defconst overlay-fix-version "1.0")

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
;; In GNU Emacs versions before 21 when the overlay 'face property is
;; nil it can disable the overlayed text face property.  The most
;; visible issue is that overlayed text is not syntax highlighted.
;; This library advices the built-in functions `make-overlay',
;; `overlay-put' and `overlay-get' to ensure that the overlay 'face
;; property will never be nil but `overlay-fix-empty-face' instead.
;; And that callers will continue to see nil for the corresponding
;; 'face property.

;;; History:
;; 

;;; Code:
(if (or (featurep 'xemacs) (> emacs-major-version 20))
    ;; Not needed for Emacs since v21 nor XEmacs
    nil

  ;; Define an empty face
  (make-empty-face 'overlay-fix-empty-face)

  (defadvice make-overlay (after after-make-overlay activate)
    "Ensure overlay 'face property is never nil."
    (or (overlay-get ad-return-value 'face)
        (overlay-put ad-return-value 'face 'overlay-fix-empty-face)))

  (defadvice overlay-put (around before-overlay-put activate)
    "Ensure overlay 'face property is never nil."
    (if (and (eq (ad-get-arg 1) 'face)
             (null (ad-get-arg 2)))
        (ad-set-arg 2 'overlay-fix-empty-face))
    ad-do-it
    (and  (eq (ad-get-arg 1) 'face)
          (eq (ad-get-arg 2) 'overlay-fix-empty-face)
          (setq ad-return-value nil)))
  
  (defadvice overlay-get (after after-overlay-get activate)
    "Ensure overlay 'face property is never nil."
    (if (and (eq (ad-get-arg 1) 'face)
             (eq ad-return-value 'overlay-fix-empty-face))
        (setq ad-return-value nil)))

  )

(provide 'overlay-fix)

;;; overlay-fix.el ends here
