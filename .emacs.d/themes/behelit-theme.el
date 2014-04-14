;;; behelit theme
;;; version: 2.4

;; Copyright (C) 2012-2014 oblique

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Installation:
;;
;; put this file in ~/.emacs.d/themes and add the following
;; in your ~/.emacs or ~/.emacs.d/init.el
;;
;;    (add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
;;    (load-theme 'behelit t)
;;
;; Don't forget that the theme requires Emacs 24.

(deftheme behelit
  "Behelit, a heavily modified molokai-based theme.")

(let ((class '((class color) (min-colors 89))))
  (custom-theme-set-faces
   'behelit

   `(default ((,class (:foreground "#5fff5f" :background "#101010"))))
   `(cursor ((,class (:background "#5fff5f"))))

   `(vertical-border ((,class (:foreground "#1c1c1c" :background "#1c1c1c"))))
   `(fringe ((,class (:foreground "#b03060" :background "#1c1c1c"))))
   `(mode-line ((,class (:foreground "#5f5faf" :background "#1c1c1c" :box nil
				     :underline nil :weight normal :slant normal))))
   `(mode-line-inactive ((,class (:foreground "#585858" :background "#1c1c1c" :box nil))))
   `(mode-line-highlight ((,class (:box nil :underline t))))
   `(linum ((,class (:background nil :foreground "#5f87af"
				 :underline nil :slant normal :weight normal))))
   `(menu ((,class (:foreground "#5f5faf" :background "#1c1c1c" :inverse-video nil))))
   `(minibuffer-prompt ((,class (:foreground "#5f87d7" :weight bold))))
   `(region ((,class (:background "#1c1c1c"))))
   `(button ((,class (:foreground "#5f87d7"))))
   `(trailing-whitespace ((,class (:background "#ff005f"))))
   `(escape-glyph ((,class (:foreground "#ffff5f"))))
   `(nobreak-space ((,class (:foreground "#ffff5f"))))

   `(show-paren-match-face ((,class (:foreground "#000000" :background "#ff875f"))))
   `(show-paren-mismatch-face ((,class (:foreground "#000000" :background "#d7005f"))))

   ;;; faces used by ggtags.el
   `(ggtags-global-line ((,class (:inherit nil :underline t))))
   `(compilation-line-number ((,class (:foreground "#d7005f"))))
   `(compilation-info ((,class (:foreground "#5f87d7"))))
   `(compilation-error ((,class (:foreground "#d7005f"))))
   `(compilation-mode-line-exit ((,class (:foreground "#5f87d7"))))
   `(compilation-mode-line-fail ((,class (:foreground "#d7005f"))))
   `(error ((,class (:foreground "#d7005f"))))
   `(success ((,class (:foreground "#00ff87"))))

   ;;; ediff
   `(ediff-even-diff-A ((,class (:foreground "#000000" :background "#5fffaf" :weight normal))))
   `(ediff-odd-diff-A ((,class (:foreground "#000000" :background "#5fffaf" :weight normal))))
   `(ediff-current-diff-A ((,class (:foreground "#000000" :background "#00ff87" :weight normal))))
   `(ediff-fine-diff-A ((,class (:foreground "#000000" :background "#d70087" :weight normal))))

   `(ediff-even-diff-B ((,class (:foreground "#000000" :background "#5f87d7" :weight normal))))
   `(ediff-odd-diff-B ((,class (:foreground "#000000" :background "#5f87d7" :weight normal))))
   `(ediff-current-diff-B ((,class (:foreground "#000000" :background "#5f5fff" :weight normal))))
   `(ediff-fine-diff-B ((,class (:foreground "#000000" :background "#d70087" :weight normal))))

   `(ediff-even-diff-C ((,class (:foreground "#000000" :background "#af87ff" :weight normal))))
   `(ediff-odd-diff-C ((,class (:foreground "#000000" :background "#af87ff" :weight normal))))
   `(ediff-current-diff-C ((,class (:foreground "#000000" :background "#af5fff" :weight normal))))
   `(ediff-fine-diff-C ((,class (:foreground "#000000" :background "#d70087" :weight normal))))

   `(ediff-even-diff-Ancestor ((,class (:foreground "#000000" :background "#ff875f" :weight normal))))
   `(ediff-odd-diff-Ancestor ((,class (:foreground "#000000" :background "#ff875f" :weight normal))))
   `(ediff-current-diff-Ancestor ((,class (:foreground "#000000" :background "#ff5f5f" :weight normal))))
   `(ediff-fine-diff-Ancestor ((,class (:foreground "#000000" :background "#d70087" :weight normal))))

   ;;; popup.el
   `(popup-face ((,class (:foreground "#585858" :background "#1c1c1c" :weight normal))))
   `(popup-isearch-match ((,class (:foreground "#000000" :background "#af87ff" :weight normal))))
   `(popup-menu-face ((,class (:foreground "#585858" :background "#1c1c1c" :weight normal))))
   `(popup-menu-mouse-face ((,class (:foreground "#000000" :background "#ff5f5f" :weight normal))))
   `(popup-menu-selection-face ((,class (:foreground "#000000" :background "#ff5f5f" :weight normal))))
   `(popup-menu-summary-face ((,class (:foreground "#585858" :background "#1c1c1c" :weight normal))))
   `(popup-scroll-bar-background-face ((,class (:background "#1c1c1c"))))
   `(popup-scroll-bar-foreground-face ((,class (:background "#af87d7"))))
   `(popup-summary-face ((,class (:foreground "#585858" :background "#1c1c1c" :weight normal))))
   `(popup-tip-face ((,class (:foreground "#ff5faf" :background "#1c1c1c" :weight normal))))

   ;;; auto-complete.el
   `(ac-candidate-mouse-face ((,class (:foreground "#000000" :background "#ff5f5f" :weight normal))))
   `(ac-completion-face ((,class (:foreground "#af87d7" :weight normal :underline t))))
   `(ac-candidate-face ((,class (:foreground "#5c5c5c" :background "#1c1c1c" :weight normal))))
   `(ac-selection-face ((,class (:foreground "#000000" :background "#ff5f5f" :weight normal))))
   `(ac-gtags-candidate-face ((,class (:foreground "#af87d7" :background "#1c1c1c" :weight normal))))
   `(ac-gtags-selection-face ((,class (:foreground "#000000" :background "#ff5f5f" :weight normal))))
   `(ac-yasnippet-candidate-face ((,class (:foreground "#ff5f5f" :background "#1c1c1c" :weight normal))))
   `(ac-yasnippet-selection-face ((,class (:foreground "#000000" :background "#ff5f5f" :weight normal))))
   `(ac-irony-candidate-face ((,class (:foreground "#ff5f5f" :background "#1c1c1c" :weight normal))))
   `(ac-irony-selection-face ((,class (:foreground "#000000" :background "#ff5f5f" :weight normal))))

   ;;; highlight
   `(isearch ((,class (:foreground "#000000" :background "#ff875f" :weight normal))))
   `(isearch-fail ((,class (:foreground "#d7005f" :background nil :weight bold))))
   `(lazy-highlight ((,class (:foreground "#000000" :background "#ffff5f" :weight normal))))
   `(highlight ((,class (:foreground "#000000" :background "#5f87d7"))))
   `(hi-black ((,class (:background "#1c1c1c" :foreground "#3a3a3a"))))
   `(hi-black-b ((,class (:background "#3a3a3a" :foreground "#1c1c1c" :weight bold))))
   `(hi-black-hb ((,class (:background "#3a3a3a" :foreground "#1c1c1c" :weight bold :height 1.50))))
   `(hi-blue ((,class (:background "#0087ff" :foreground "#1c1c1c"))))
   `(hi-blue-b ((,class (:foreground "#0087ff" :weight bold))))
   `(hi-green ((,class (:background "#00ff87" :foreground "#1c1c1c"))))
   `(hi-green-b ((,class (:foreground "#00ff87" :weight bold))))
   `(hi-pink ((,class (:background "#d7005f" :foreground "#1c1c1c"))))
   `(hi-red-b ((,class (:foreground "#d7005f" :weight bold))))
   `(hi-yellow ((,class (:background "#ffff5f" :foreground "#1c1c1c"))))
   `(hl-line ((,class (:background "#1c1c1c" :inherit nil))))
   `(match ((,class (:background "#ffff5f" :foreground "#4e4e4e" :inherit nil))))

   ;;; tabbar
   `(tabbar-default ((,class (:background "#1c1c1c" :box nil :underline nil :height 1.0 :weight normal :inherit nil))))
   `(tabbar-unselected ((,class (:background "#1c1c1c" :foreground "#585858"
					     :box nil :underline nil :weight normal))))
   `(tabbar-selected ((,class (:background "#1c1c1c" :foreground "#d7005f"
					   :box nil :underline nil :weight bold))))
   `(tabbar-button ((,class (:background "#1c1c1c" :foreground "#585858"
					 :box nil :underline nil))))

   ;;; font lock
   `(font-lock-builtin-face ((,class (:foreground "#8787af"))))
   `(font-lock-comment-face ((,class (:foreground "#585858"))))
   `(font-lock-comment-delimiter-face ((,class (:foreground "#585858"))))
   `(font-lock-constant-face ((,class (:foreground "#af87ff"))))
   `(font-lock-doc-face ((,class (:foreground "#5f5f87"))))
   `(font-lock-function-name-face ((,class (:foreground "#af87ff" :weight bold))))
   `(font-lock-keyword-face ((,class (:foreground "#d7005f" :weight bold))))
   `(font-lock-negation-char-face ((,class (:weight bold))))
   `(font-lock-preprocessor-face ((,class (:foreground "#87ff5f" :weight bold))))
   `(font-lock-string-face ((,class (:foreground "#ffff87"))))
   `(font-lock-type-face ((,class (:foreground "#5f87d7"))))
   `(font-lock-variable-name-face ((,class (:foreground "#d7005f" :weight bold))))
   `(font-lock-warning-face ((,class (:foreground "#d7005f" :weight bold))))
   `(c-annotation-face ((,class (:inherit font-lock-constant-face))))
   `(font-lock-custom-number-face ((,class (:foreground "#af87ff"))))
   `(font-lock-custom-todo-face ((,class (:foreground "#af87ff" :weight bold))))

   ;;; diff
   `(diff-added ((,class (:foreground "#5f87ff" :background nil :weight bold))))
   `(diff-removed ((,class (:foreground "#d7005f" :background nil :weight bold))))
   `(diff-indicator-added ((,class (:inherit diff-added))))
   `(diff-indicator-removed ((,class (:inherit diff-removed))))
   `(diff-refine-added ((,class (:inherit diff-added :weight normal
					  :foreground "#000000" :background "#5f87ff"))))
   `(diff-refine-removed ((,class (:inherit diff-removed :weight normal
					    :foreground "#000000" :background "#d7005f"))))
   `(diff-context ((,class (:foreground "#5fff5f"))))
   `(diff-file-header ((,class (:foreground "#af87ff" :background nil))))
   `(diff-header ((,class (:foreground "#af87ff" :background nil))))
   `(diff-hunk-header ((,class (:foreground "#ff5f5f" :background nil))))

   ;;; rfc
   `(rfc-node ((,class (:bold t :foreground "#af87ff"))))
   `(rfc-xref ((,class (:bold t :foreground "#af87ff"))))

   ;;; markdown-mode
   `(markdown-italic-face ((,class (:slant italic))))
   `(markdown-bold-face ((,class (:weight bold))))
   `(markdown-header-face ((,class (:weight normal))))
   `(markdown-header-face-1 ((,class (:foreground "#5f87ff"))))
   `(markdown-header-face-2 ((,class (:foreground "#d7005f"))))
   `(markdown-header-face-3 ((,class (:foreground "#87ff5f"))))
   `(markdown-header-face-4 ((,class (:foreground "#af87ff"))))
   `(markdown-header-face-5 ((,class (:foreground "#ffff87"))))
   `(markdown-header-face-6 ((,class (:foreground "#5f87ff"))))
   `(markdown-inline-code-face ((,class (:foreground "#5f87ff"))))
   `(markdown-list-face ((,class (:foreground "#87ff5f"))))
   `(markdown-blockquote-face ((,class (:slant italic))))
   `(markdown-pre-face ((,class (:foreground "#af87ff"))))
   `(markdown-link-face ((,class (:foreground "#5f87ff"))))
   `(markdown-reference-face ((,class (:foreground "#5f87ff"))))
   `(markdown-url-face ((,class (:foreground "#ffff87"))))
   `(markdown-link-title-face ((,class (:foreground "#d7005f"))))
   `(markdown-comment-face ((,class (:foreground "#585858"))))
   `(markdown-math-face ((,class (:foreground "#af87ff" :slant italic))))

   ;;; outline-mode
   `(outline-1 ((,class (:foreground "#5f87ff"))))
   `(outline-2 ((,class (:foreground "#d7005f"))))
   `(outline-3 ((,class (:foreground "#87ff5f"))))
   `(outline-4 ((,class (:foreground "#af87ff"))))
   `(outline-5 ((,class (:foreground "#ffff87"))))
   `(outline-6 ((,class (:foreground "#5f87ff"))))
   `(outline-7 ((,class (:foreground "#d7005f"))))
   `(outline-8 ((,class (:foreground "#87ff5f"))))

   ))

;;; popup.el: disable cursor color changing
(setq popup-isearch-cursor-color nil)

;;; additional highlightings
(unless (boundp 'font-lock-custom-number-face)
  (progn
    (make-face 'font-lock-custom-number-face)
    (setq font-lock-custom-number-face 'font-lock-custom-number-face)))

(unless (boundp 'font-lock-custom-todo-face)
  (progn
    (make-face 'font-lock-custom-todo-face)
    (setq font-lock-custom-todo-face 'font-lock-custom-todo-face)))

(defun add-font-locks ()
  (font-lock-add-keywords nil
     '(
       ("\\<\\(\\([0-9]+\\.\\)?[0-9]+\\([eE][+-]?[0-9]*\\)?\\([uU]?[lL]\\{0,2\\}\\|[lL]\\{0,2\\}[uU]?\\)\\)\\>" . font-lock-custom-number-face)
       ("\\<\\(0[xX][0-9a-fA-F]+\\)\\>" . font-lock-custom-number-face)
       ))
  (font-lock-add-keywords nil '(("\\<\\(FIXME\\):" 1 font-lock-warning-face t)))
  (font-lock-add-keywords nil '(("\\<\\(TODO\\):" 1 font-lock-custom-todo-face t))))


;; enable additional font-locks for the following modes
(add-hook 'c-mode-common-hook 'add-font-locks)


(provide-theme 'behelit)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; behelit-theme.el ends here.
