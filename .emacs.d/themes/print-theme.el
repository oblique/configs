(deftheme print
  "Color theme for code printing")

(let ((class '((class color) (min-colors 89))))
  (custom-theme-set-faces
   'print

   `(default ((,class (:foreground "#000000" :background "#ffffff"))))
   `(font-lock-builtin-face ((,class (:foreground "#66aa11")))) ; green
   `(font-lock-comment-face ((,class (:foreground "#465457" :slant italic)))) ; gray
   `(font-lock-comment-delimiter-face ((,class (:foreground "#465457" :slant italic))))
   `(font-lock-constant-face ((,class (:foreground "#7e40a5")))) ; purple
   `(font-lock-doc-face ((,class (:foreground "#c47f2c" :slant italic)))) ; yellow
   `(font-lock-function-name-face ((,class (:foreground "#960050" :slant italic)))) ; red
   `(font-lock-keyword-face ((,class (:foreground "#30309b")))) ; blue
   `(font-lock-negation-char-face ((,class (:weight bold))))
   `(font-lock-preprocessor-face ((,class (:foreground "#66aa11")))) ; green
   `(font-lock-regexp-grouping-backslash ((,class (:weight bold))))
   `(font-lock-regexp-grouping-construct ((,class (:weight bold))))
   `(font-lock-string-face ((,class (:foreground "#c47f2c")))) ; yellow
   `(font-lock-type-face ((,class (:foreground "#30309b")))) ; blue
   `(font-lock-variable-name-face ((,class (:foreground "#960050")))) ; red
   `(font-lock-warning-face ((,class (:foreground "#F8F8F2" :background "#333333"))))
   ;; check init.el for font-lock-number-face
   `(font-lock-number-face ((,class (:foreground "#7e40a5"))))

   ))


(provide-theme 'print)

;; Local Variables:
;; no-byte-compile: t
;; End:
