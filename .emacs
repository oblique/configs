(add-to-list 'load-path "~/.emacs.d")
(add-to-list 'load-path "~/.emacs.d/emhacks")

; load my rxvt.el
(unless (or noninteractive initial-window-system)
    (if (string-match "^rxvt.*" (getenv "TERM"))
        (progn 
            (setq term-file-prefix nil)
            (require 'rxvt)
            (terminal-init-rxvt))))

; various
(menu-bar-mode nil)
(tool-bar-mode nil)
(toggle-scroll-bar nil)
(global-linum-mode 1)
(column-number-mode 1)
(global-auto-revert-mode 1)
(setq inhibit-splash-screen t)
(defalias 'yes-or-no-p 'y-or-n-p)
(setq explicit-shell-file-name "/bin/bash")

; session manager
(require 'desktop-menu)

; coding
(setq c-default-style "linux")
(setq c-backspace-function 'backward-delete-char)
(define-key global-map (kbd "RET") 'newline-and-indent) ; auto-indent

; Tabbing support options
(require 'tabbar)
(tabbar-mode 1)
(setq tabbar-separator (quote (" ")))
(setq tabbar-use-images nil)

; undo
(require 'undo-tree)
(global-undo-tree-mode)

; fonts
(if (or noninteractive initial-window-system)
    (setq default-frame-alist '((font . "Inconsolata-10"))))

; theme
(require 'color-theme)
(require 'color-theme-molokai-ob)
(setq color-theme-is-global t)
(color-theme-molokai-ob)

; move to previous window
(defun other-window-backward (&optional n)
    "Select Nth previous window."
    (interactive "P")
    (other-window (- (prefix-numeric-value n))))

; key bindings
(global-set-key (kbd "M-9") 'kill-whole-line)
(global-set-key (kbd "M-l") 'tabbar-forward)
(global-set-key (kbd "M-j") 'tabbar-backward)
(global-set-key (kbd "C-^") 'undo-tree-redo) ; redo with C-6, undo with C-7 (in terminal)
(global-set-key (kbd "C-x p") 'other-window-backward)
