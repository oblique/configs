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
(require 'linum-ex)
(setq linum-disabled-modes-list '(eshell-mode apropos-mode compilation-mode
                                imenu-tree-mode fundamental-mode term-mode
                                completion-list-mode tags-tree-mode help-mode
                                dired-mode dirtree-mode desktop-menu-mode
                                Buffer-menu-mode))
(global-linum-mode 1)
(column-number-mode 1)
(global-auto-revert-mode 1)
(unless initial-window-system
  (xterm-mouse-mode 1))
(setq inhibit-splash-screen t)
(defalias 'yes-or-no-p 'y-or-n-p)
(setq explicit-shell-file-name "/bin/bash")

; sticky windows
(require 'sticky-windows)
(global-set-key (kbd "C-x 0") 'sticky-window-delete-window)
(global-set-key (kbd "C-x 1") 'sticky-window-delete-other-windows)
(global-set-key (kbd "C-x 9") 'sticky-window-keep-window-visible)

; session manager
(require 'desktop-menu)

; tree widgets
(require 'tree-mode)
(require 'imenu-tree)
(setq imenu-tree-auto-update t)
(setq imenu-tree-update-interval 1)
(setq imenu-tree-windata '(frame left 0.15 nil))
(require 'dirtree)
(setq dirtree-windata '(frame left 0.15 nil))

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
(if initial-window-system
    (setq default-frame-alist '((font . "Inconsolata-10"))))

; theme
(require 'color-theme)
(require 'color-theme-molokai-ob)
(setq color-theme-is-global t)
(color-theme-molokai-ob)

; functions
(defun other-window-backward (&optional n)
    "Select Nth previous window."
    (interactive "P")
    (other-window (- (prefix-numeric-value n))))

(defun my-dirtree ()
  (interactive)
  (dirtree (expand-file-name default-directory) nil)
  (select-window (get-buffer-window dirtree-buffer)))

(defun my-imenu-tree ()
  (interactive)
  (imenu-tree nil)
  (select-window (get-buffer-window imenu-tree-buffer)))

(defun ob-tree ()
  (interactive)
  (let ((buf (current-buffer)))
    (delete-other-windows)
    (imenu-tree nil)
    (delete-window (selected-window))
    (dirtree (expand-file-name default-directory) nil)
    (delete-window (selected-window))
    (setq newwin (split-window nil 30 t))
    (split-window-vertically nil)
    (switch-to-buffer imenu-tree-buffer)
    (sticky-window-keep-window-visible)
    (other-window 1)
    (switch-to-buffer dirtree-buffer)
    (sticky-window-keep-window-visible)
    (select-window (get-buffer-window buf))))

; key bindings
(global-set-key (kbd "M-9") 'kill-whole-line)
(global-set-key (kbd "M-l") 'tabbar-forward)
(global-set-key (kbd "M-j") 'tabbar-backward)
(global-set-key (kbd "C-^") 'undo-tree-redo) ; redo with C-6, undo with C-7 (in terminal)
(global-set-key (kbd "C-x p") 'other-window-backward)
(global-set-key (kbd "C-c m") 'my-imenu-tree)
(global-set-key (kbd "C-c d") 'my-dirtree)
(global-set-key (kbd "C-c o") 'ob-tree)
