; compile .el files if .elc does not exist
(byte-recompile-directory "~/.emacs.d" 0)

; load my rxvt.el
(unless (or noninteractive initial-window-system)
    (if (string= (getenv "TERM") "rxvt-unicode-256color")
        (progn 
            (setq term-file-prefix nil)
            (load "~/.emacs.d/rxvt.elc")
            (terminal-init-rxvt))))

(add-to-list 'load-path "~/.emacs.d")
(add-to-list 'load-path "/usr/share/emacs/site-lisp/emhacks")

; various
(menu-bar-mode nil)
(tool-bar-mode nil)
(toggle-scroll-bar nil)
(setq inhibit-splash-screen t)
(setq c-default-style "linux")
(global-linum-mode 1)
(defalias 'yes-or-no-p 'y-or-n-p)

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
(color-theme-initialize)
(setq color-theme-is-global t)
(load "color-theme-molokai-ob")
(color-theme-molokai-ob)

; key bindings
(global-set-key (kbd "M-9") 'kill-whole-line)
(global-set-key (kbd "M-l") 'tabbar-forward)
(global-set-key (kbd "M-j") 'tabbar-backward)
(global-set-key (kbd "C-^") 'undo-tree-redo) ; redo with C-6, undo with C-7 (in terminal)
