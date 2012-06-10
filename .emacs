(add-to-list 'load-path "~/.emacs.d")
(setq byte-compile-warnings nil)
(byte-recompile-directory "~/.emacs.d")
(byte-recompile-directory "~/.emacs.d" 0)
(setq byte-compile-warnings t)

; load my rxvt.el
(unless (or noninteractive initial-window-system)
    (if (string-match "^rxvt.*" (getenv "TERM"))
        (progn
            (setq term-file-prefix nil)
            (require 'rxvt)
            (terminal-init-rxvt))))

; various
(defvar my-minor-mode-map (make-keymap) "my-minor-mode keymap.")
(menu-bar-mode nil)
(tool-bar-mode nil)
(toggle-scroll-bar nil)
(require 'linum-ex)
(setq linum-disabled-modes-list '(eshell-mode apropos-mode compilation-mode
                                imenu-tree-mode fundamental-mode term-mode
                                completion-list-mode tags-tree-mode help-mode
                                dired-mode dirtree-mode desktop-menu-mode
                                Buffer-menu-mode Man-mode Custom-mode))
(global-linum-mode 1)
(column-number-mode 1)
(global-auto-revert-mode 1)
(show-paren-mode 1)
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
(setq Man-width 90)
(setq initial-scratch-message nil)
(setq explicit-shell-file-name "/bin/bash")
(setq-default show-trailing-whitespace t)
(push '("." . "~/.emacs-backups") backup-directory-alist)
(defalias 'yes-or-no-p 'y-or-n-p)
(require 'smooth-scrolling)
(require 'show-point-mode)
(define-globalized-minor-mode global-show-point-mode show-point-mode
  (lambda ()
    (show-point-mode t)))
(global-show-point-mode t)
(show-point-mode 1)
(require 'htmlize)
(require 'lua-mode)
(require 'highlight-parentheses)
(define-globalized-minor-mode global-highlight-parentheses-mode
  highlight-parentheses-mode
  (lambda ()
    (highlight-parentheses-mode t)))
(global-highlight-parentheses-mode t)
(require 'arduino-mode)

; RFC
; you can download RFCs from http://www.rfc-editor.org/download.html
(require 'rfc)
(setq rfc-archive-alist (list (file-truename "~/rfc")))

; sticky windows
(require 'sticky-windows)
(define-key my-minor-mode-map (kbd "C-x 0") 'sticky-window-delete-window)
(define-key my-minor-mode-map (kbd "C-x 1") 'sticky-window-delete-other-windows)
(define-key my-minor-mode-map (kbd "C-x 9") 'sticky-window-keep-window-visible)

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

; tags
(require 'etags-select)
(global-set-key "\M-?" 'etags-select-find-tag-at-point)
(global-set-key "\M-." 'etags-select-find-tag)

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
(setq ct-theme 'color-theme-molokai-ob) ; set your theme here
(funcall ct-theme)

; special highlighting for numbers
(make-face 'font-lock-number-face)
(set-face-attribute 'font-lock-number-face nil :inherit font-lock-constant-face)
(setq font-lock-number-face 'font-lock-number-face)
(defvar font-lock-number "[0-9]+\\([eE][+-]?[0-9]*\\)?\\([uU]?[lL]\\{0,2\\}\\|[lL]\\{0,2\\}[uU]?\\)")
(defvar font-lock-hexnumber "0[xX][0-9a-fA-F]+")
(defun add-font-lock-numbers ()
  (font-lock-add-keywords nil (list
			       (list (concat "\\<\\(" font-lock-number "\\)\\>" )
				     0 font-lock-number-face)
			       (list (concat "\\<\\(" font-lock-hexnumber "\\)\\>" )
				     0 font-lock-number-face)
			       )))

(add-hook 'c-mode-common-hook 'add-font-lock-numbers)

; cups pdf printer
(setq ps-printer-name "Virtual_PDF_Printer")
(setq ps-printer-name-option "-P")
(setq ps-lpr-command "/usr/bin/lpr")
(setq ps-paper-type 'a4)
(setq ps-font-size 8.5)
(setq ps-left-margin 60)
(setq ps-right-margin 40)
(setq ps-top-margin 70)
(setq ps-bottom-margin 70)
(setq ps-print-header nil)

; functions
(require 'ps-print)
(defun print-code (&optional filename)
  (interactive (list (ps-print-preprint current-prefix-arg)))
  (let ((old-ps-line-number ps-line-number)
	 (old-ps-line-number-font ps-line-number-font)
	 (old-ps-line-number-font-size ps-line-number-font-size)
	 (old-ps-font-family ps-font-family)
	 (old-highlight-parentheses-mode highlight-parentheses-mode))
    (setq ps-line-number t)
    (setq ps-font-family 'Courier)
    (setq ps-line-number-font "Courier")
    (setq ps-line-number-font-size ps-font-size)
    (highlight-parentheses-mode nil)
    (color-theme-print)
    (ps-print-buffer-with-faces filename)
    (funcall ct-theme)
    (setq ps-line-number old-ps-line-number)
    (setq ps-line-number-font old-ps-line-number-font)
    (setq ps-line-number-font-size old-ps-line-number-font-size)
    (setq ps-font-family old-ps-font-family)
    (highlight-parentheses-mode old-highlight-parentheses-mode)))

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
      (call-process-region (region-beginning) (region-end) "xclip" nil 0 nil "-i" "-selection" "clipboard")
      (message "Copy region to clipboard!")
      (deactivate-mark))
    (message "No region active. Can't copy to clipboard!")))

(defun xclip-cut ()
  (interactive)
  (if (region-active-p)
    (progn
      (call-process-region (region-beginning) (region-end) "xclip" t 0 nil "-i" "-selection" "clipboard")
      (message "Cut region to clipboard!"))
    (message "No region active. Can't cut to clipboard!")))

(defun desktop-save-man ()
  (insert "\n;; Man section\n")
  (let* ((list (buffer-list))
	 (buffer (car list))
	 val)
    (while buffer
      (if (with-current-buffer buffer (boundp 'Man-arguments))
	  (progn
	    (with-current-buffer buffer (setq val Man-arguments))
	    (if val
		(progn
		  (insert "(man \"")
		  (insert val)
		  (insert "\")\n")))))
      (setq buffer (car list))
      (setq list (cdr list)))))

(defun desktop-save-rfc ()
  (insert "\n;; RFC section\n")
  (let* ((list (buffer-list))
	 (buffer (car list))
	 val)
    (while buffer
      (if (with-current-buffer buffer (boundp 'rfc-article-number))
	  (progn
	    (with-current-buffer buffer (setq val rfc-article-number))
	    (if (> val 0)
		(progn
		  (insert "(rfc-goto-number ")
		  (insert (number-to-string val))
		  (insert ")\n")))))
      (setq buffer (car list))
      (setq list (cdr list)))))

(defun shifttext-tab-right (tabs)
  (interactive "P")
  (unless tabs
      (setq tabs 1))
  (let (begin end)
    (if (mark)
	(setq begin (region-beginning)
	      end (region-end))
      (setq begin (line-beginning-position)
	     end (line-end-position)))
      (save-excursion
	(indent-rigidly begin end (* tab-width tabs))
	(setq deactivate-mark nil))))

(defun shifttext-tab-left (tabs)
  (interactive "P")
  (unless tabs
    (setq tabs 1))
  (shifttext-tab-right (- tabs)))

(defun clear-tags-path ()
  (interactive)
  (setq tags-file-name nil)
  (setq tags-table-list nil))

(defun show-file-path ()
  "Show the full path file name in the minibuffer."
  (interactive)
  (message (buffer-file-name)))

; hooks
(add-hook 'desktop-save-hook 'desktop-save-man)
(add-hook 'desktop-save-hook 'desktop-save-rfc)

; key bindings
(global-set-key (kbd "RET") 'newline-and-indent) ; auto-indent
(global-set-key (kbd "M-9") 'kill-whole-line)
(global-set-key (kbd "M->") 'shifttext-tab-right)
(global-set-key (kbd "M-<") 'shifttext-tab-left)
(global-set-key (kbd "C-c p") 'show-file-path)
(global-set-key (kbd "C-^") 'undo-tree-redo) ; redo with C-6, undo with C-7 (in terminal)
(global-set-key (kbd "C-x p") 'other-window-backward)
(define-key my-minor-mode-map (kbd "<clearline>") (key-binding (kbd "<C-end>"))) ; <clearline> (in terminal) == <C-end>
(define-key my-minor-mode-map (kbd "M-l") 'tabbar-forward)
(define-key my-minor-mode-map (kbd "M-j") 'tabbar-backward)
(define-key my-minor-mode-map (kbd "ESC <up>") 'windmove-up) ; M-up in terminal
(define-key my-minor-mode-map (kbd "ESC <down>") 'windmove-down) ; M-down in terminal
(define-key my-minor-mode-map (kbd "ESC <right>") 'windmove-right) ; M-right in terminal
(define-key my-minor-mode-map (kbd "ESC <left>") 'windmove-left) ; M-left in terminal
(define-key my-minor-mode-map (kbd "<M-up>") 'windmove-up) ; M-up in gui
(define-key my-minor-mode-map (kbd "<M-down>") 'windmove-down) ; M-down in gui
(define-key my-minor-mode-map (kbd "<M-right>") 'windmove-right) ; M-right in gui
(define-key my-minor-mode-map (kbd "<M-left>") 'windmove-left) ; M-left in gui
(define-key my-minor-mode-map (kbd "C-c f") 'my-imenu-tree)
(define-key my-minor-mode-map (kbd "C-c d") 'my-dirtree)
(define-key my-minor-mode-map (kbd "C-c o") 'ob-tree)
(define-key my-minor-mode-map (kbd "C-c q") 'delete-other-windows)
(define-key my-minor-mode-map (kbd "C-c z") 'my-eshell)
(define-key my-minor-mode-map (kbd "C-c Z") 'my-eshell-kill)
(define-key my-minor-mode-map (kbd "C-c s") 'desktop-menu)
(define-key my-minor-mode-map (kbd "C-c v") 'xclip-paste)
(define-key my-minor-mode-map (kbd "C-c c") 'xclip-copy)
(define-key my-minor-mode-map (kbd "C-c x") 'xclip-cut)
(define-key my-minor-mode-map (kbd "C-c m") 'man)
(define-key my-minor-mode-map (kbd "C-c r") 'rfc-index)

(define-minor-mode my-minor-mode
    "A minor mode so that my key settings aren't shadowed by other major/minor modes"
    t "" 'my-minor-mode-map)
