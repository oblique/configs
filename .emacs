(add-to-list 'load-path "~/.emacs.d")

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
                                Buffer-menu-mode Man-mode))
(global-linum-mode 1)
(column-number-mode 1)
(global-auto-revert-mode 1)
(show-paren-mode 1)
(recentf-mode 1)
(setq inhibit-splash-screen t)
(defalias 'yes-or-no-p 'y-or-n-p)
(setq explicit-shell-file-name "/bin/bash")
(require 'smooth-scrolling)
(require 'rainbow-delimiters)
(global-rainbow-delimiters-mode)
(setq Man-width 90)

; RFC
; you can download RFCs from http://www.rfc-editor.org/download.html
(require 'rfc)
(setq rfc-archive-alist (list (file-truename "~/rfc")))

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
(color-theme-initialize)
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
    (unless (or (string-match "^\\*.+\\*$" (buffer-name))
                (eq major-mode 'fundamental-mode))
      (progn
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
        (select-window (get-buffer-window buf))))))

(defun my-eshell ()
  (interactive)
  (let ((active-eshell-window nil) newwin)
    (walk-windows (function (lambda (window)
                              (if (string= (buffer-name (window-buffer window)) "*eshell*")
                                (setq active-eshell-window window)))))
    (if active-eshell-window
      (select-window active-eshell-window)
      (let ((buf (current-buffer)) eshellbuf)
        (eshell)
        (setq eshellbuf (current-buffer))
        (switch-to-buffer buf)
        (setq newwin (split-window nil 40))
        (select-window newwin)
        (switch-to-buffer eshellbuf)
        (sticky-window-keep-window-visible)))))

(defun my-eshell-kill ()
  (interactive)
  (let ((active-eshell-window nil))
    (walk-windows (function (lambda (window)
                              (if (string= (buffer-name (window-buffer window)) "*eshell*")
                                (setq active-eshell-window window)))))
    (if active-eshell-window
      (delete-window active-eshell-window))))

(defun xclip-paste ()
  (interactive)
  (shell-command "xclip -o -selection clipboard" 0 shell-command-default-error-buffer))

(defun xclip-copy ()
  (interactive)
  (if (region-active-p)
    (progn
      (shell-command-on-region (region-beginning) (region-end) "xclip -i -selection clipboard")
      (message "Copy region to clipboard!")
      (deactivate-mark))
    (message "No region active. Can't copy to clipboard!")))

(defun xclip-cut ()
  (interactive)
  (if (region-active-p)
    (progn
      (shell-command-on-region (region-beginning) (region-end) "xclip -i -selection clipboard")
      (message "Cut region to clipboard!")
      (delete-region (region-beginning) (region-end)))
    (message "No region active. Can't cut to clipboard!")))

; key bindings
(global-set-key (kbd "M-9") 'kill-whole-line)
(global-set-key (kbd "M-l") 'tabbar-forward)
(global-set-key (kbd "M-j") 'tabbar-backward)
(global-set-key (kbd "ESC <up>") 'windmove-up) ; M-up
(global-set-key (kbd "ESC <down>") 'windmove-down) ; M-down
(global-set-key (kbd "ESC <right>") 'windmove-right) ; M-right
(global-set-key (kbd "ESC <left>") 'windmove-left) ; M-left
(global-set-key (kbd "C-^") 'undo-tree-redo) ; redo with C-6, undo with C-7 (in terminal)
(global-set-key (kbd "C-x p") 'other-window-backward)
(global-set-key (kbd "C-c f") 'my-imenu-tree)
(global-set-key (kbd "C-c d") 'my-dirtree)
(global-set-key (kbd "C-c o") 'ob-tree)
(global-set-key (kbd "C-c q") 'delete-other-windows)
(global-set-key (kbd "C-c z") 'my-eshell)
(global-set-key (kbd "C-c Z") 'my-eshell-kill)
(global-set-key (kbd "C-c s") 'desktop-menu)
(global-set-key (kbd "C-c v") 'xclip-paste)
(global-set-key (kbd "C-c c") 'xclip-copy)
(global-set-key (kbd "C-c x") 'xclip-cut)
(global-set-key (kbd "C-c m") 'man)
(global-set-key (kbd "C-c r") 'rfc-index)
