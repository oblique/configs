(eval-when-compile
  (require 'color-theme))

(defun color-theme-print ()
  "Color theme for code printing"
  (interactive)
  (color-theme-install
   '(color-theme-print
     ((foreground-color . "#000000") ; black
      (background-color . "#ffffff") ; white
      (background-mode . light))
     (default ((t (:foreground "#000000" :background "#ffffff"))))
     (font-lock-builtin-face ((t (:foreground "#66aa11")))) ; green
     (font-lock-comment-face ((t (:foreground "#465457" :slant italic)))) ; gray
     (font-lock-comment-delimiter-face ((t (:foreground "#465457" :slant italic))))
     (font-lock-constant-face ((t (:foreground "#7e40a5")))) ; purple
     (font-lock-doc-face ((t (:foreground "#c47f2c" :slant italic)))) ; yellow
     (font-lock-function-name-face ((t (:foreground "#960050" :slant italic)))) ; red
     (font-lock-keyword-face ((t (:foreground "#30309b")))) ; blue
     (font-lock-negation-char-face ((t (:weight bold))))
     (font-lock-preprocessor-face ((t (:foreground "#66aa11")))) ; green
     (font-lock-regexp-grouping-backslash ((t (:weight bold))))
     (font-lock-regexp-grouping-construct ((t (:weight bold))))
     (font-lock-string-face ((t (:foreground "#c47f2c")))) ; yellow
     (font-lock-type-face ((t (:foreground "#30309b")))) ; blue
     (font-lock-variable-name-face ((t (:foreground "#960050")))) ; red
     (font-lock-warning-face ((t (:foreground "#F8F8F2" :background "#333333"))))
     )))

(provide 'color-theme-print)
