(add-to-list 'load-path "~/.emacs.d/elisp")
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(when (require 'el-get nil t)
  (el-get 'sync))

; various
(defvar my-minor-mode-map (make-keymap) "my-minor-mode keymap.")
(menu-bar-mode -1)
(when window-system
  (tool-bar-mode -1)
  (toggle-scroll-bar nil))
(column-number-mode 1)
(global-auto-revert-mode 1)
(show-paren-mode 1)
(icomplete-mode 99)
(ido-mode 1)
(delete-selection-mode 1)
(electric-pair-mode t)
(electric-indent-mode t)
(electric-layout-mode t)
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
(setq inhibit-startup-buffer-menu t)
(setq initial-scratch-message nil)
(setq message-log-max nil)
(setq fill-column 80)
(kill-buffer "*Messages*")
(setq Man-width 90)
(setq-default show-trailing-whitespace t)
(defalias 'yes-or-no-p 'y-or-n-p)
(require 'htmlize)
(require 'highlight-parentheses)
(define-globalized-minor-mode global-highlight-parentheses-mode
  highlight-parentheses-mode
  (lambda ()
    (highlight-parentheses-mode t)))
(global-highlight-parentheses-mode t)
(setq ediff-split-window-function 'split-window-horizontally)

;; set temporary directory
(setq temporary-file-directory (format "/tmp/emacs-tmp-%s/" (user-real-login-name)))
(make-directory temporary-file-directory t)

;; linum mode
(global-linum-mode 1)
(setq linum-disabled-modes-list
      '(eshell-mode apropos-mode compilation-mode term-mode
		    fundamental-mode ggtags-global-mode
		    completion-list-mode help-mode dired-mode
		    desktop-menu-mode Buffer-menu-mode Man-mode
		    Custom-mode recentf-dialog-mode occur-mode))

(add-hook 'after-change-major-mode-hook
	  '(lambda()
	     (if (member major-mode linum-disabled-modes-list)
		 (linum-mode -1))))

;;; linum separator for terminal
;; 1) add a hook to calculate the format
(unless window-system
  (add-hook 'linum-before-numbering-hook
	    (lambda ()
	      (setq-local linum-format-fmt
			  (let ((w (length (number-to-string
					    (count-lines (point-min) (point-max))))))
			    (concat "%" (number-to-string w) "d"))))))

;; 2) evaluate the format and concatenate the separetor to it
(defun linum-format-func (line)
  (concat
   (propertize (format linum-format-fmt line) 'face 'linum)
   (propertize " " 'face 'mode-line)))

;; 3) set the function pointer to linum-format
(unless window-system
  (setq linum-format 'linum-format-func))
;;;

; keep track of the recently opened file
(setq recentf-max-saved-items 200)
(setq recentf-max-menu-items 15)
(recentf-mode 1)

; modes for mutt
(add-to-list 'auto-mode-alist '("/mutt" . mail-mode))
(add-hook 'mail-mode-hook '(lambda () (auto-fill-mode)))

; disable backup/autosave
(setq backup-inhibited t)
(setq auto-save-default nil)

; session manager
(require 'desktop-menu)
(setq desktop-menu-base-filename (convert-standard-filename "session"))
(setq desktop-menu-list-file (convert-standard-filename "sessions"))
(defvar sessions-dir (expand-file-name "~/.emacs.d/sessions/"))
(make-directory sessions-dir t)
(setq desktop-menu-directory sessions-dir)

; add repositories in emacs' package manager
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

; RFC
; you can download RFCs from http://www.rfc-editor.org/download.html
(require 'rfc)
(setq rfc-archive-alist (list (file-truename "~/rfc")))

; coding
(setq c-default-style "linux")
(setq c-backspace-function 'backward-delete-char)

; ggtags (https://github.com/leoliu/ggtags)
(when (require 'ggtags nil t)
  (add-hook 'c-mode-common-hook
	    (lambda ()
	      (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
		(ggtags-mode 1)))))

; Tabbing support options
(require 'tabbar)
(tabbar-mode 1)
(setq tabbar-separator (quote (" ")))
(setq tabbar-use-images nil)

; fonts
(if window-system
    (setq default-frame-alist '((font . "Hermit:style=medium:pixelsize=14"))))

; theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'behelit t)

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
        (old-highlight-parentheses-mode highlight-parentheses-mode)
        (old-custom-enabled-themes custom-enabled-themes))
    (setq ps-line-number t)
    (setq ps-font-family 'Courier)
    (setq ps-line-number-font "Courier")
    (setq ps-line-number-font-size ps-font-size)
    (highlight-parentheses-mode -1)
    (let ((list old-custom-enabled-themes)
          x)
      (while list
        (setq x (car list))
        (setq list (cdr list))
        (disable-theme x)))
    (load-theme 'print t)
    (ps-print-buffer-with-faces filename)
    (disable-theme 'print)
    (let ((list (reverse old-custom-enabled-themes))
          x)
      (while list
        (setq x (car list))
        (setq list (cdr list))
        (load-theme x t)))
    (setq ps-line-number old-ps-line-number)
    (setq ps-line-number-font old-ps-line-number-font)
    (setq ps-line-number-font-size old-ps-line-number-font-size)
    (setq ps-font-family old-ps-font-family)
    (highlight-parentheses-mode old-highlight-parentheses-mode)))

(defun other-window-backward (&optional n)
    "Select Nth previous window."
    (interactive "P")
    (other-window (- (prefix-numeric-value n))))

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
    (if (region-active-p)
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

(defun show-file-path ()
  "Show the full path file name in the minibuffer."
  (interactive)
  (message (buffer-file-name)))

;; source: http://steve.yegge.googlepages.com/my-dot-emacs-file
(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file name new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))

(defun enable-spaces (width)
  "Use spaces instead of tabs"
  (interactive "P")
  (unless width
    (setq width 4))
  (setq tab-width width)
  (setq c-basic-offset width)
  (setq indent-tabs-mode nil)
  (if (called-interactively-p 'any)
      (message "You can retab the whole buffer by pressing C-x h M-x untabify")))

(defun enable-tabs (width)
  "Use tabs instead of spaces"
  (interactive "P")
  (unless width
    (setq width 8))
  (setq tab-width width)
  (setq c-basic-offset width)
  (setq indent-tabs-mode t)
  (if (called-interactively-p 'any)
      (message "You can retab the whole buffer by pressing C-x h M-x tabify")))

(defun hide-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

;; emulate vim's softtabstop
(defun backward-delete-whitespace-to-column ()
  "delete back to the previous column of whitespace, or as much whitespace as possible,
or just one char if that's not possible"
  (interactive)
  (if indent-tabs-mode
      (call-interactively 'backward-delete-char)
    (let ((movement (% (current-column) tab-width))
          (p (point)) sp)
      (when (= movement 0) (setq movement tab-width))
      (setq sp (- p movement))
      (when (<= sp 0) (setq sp 1))
      (save-match-data
        (if (string-match "\\w*\\(\\s-+\\)$" (buffer-substring-no-properties sp p))
            (backward-delete-char-untabify (- (match-end 1) (match-beginning 1)))
	  (call-interactively 'backward-delete-char))))))

; hooks for ARM assembly
(defun arm-asm-mode-set-comment-hook ()
  (when (string-match ".S$" (buffer-file-name))
    ;; asm files ending in .S are usually arm assembler
    (setq asm-comment-char ?@)))

(defun arm-asm-mode-hook ()
  ;; asm files ending in .S are usually arm assembler
  (when (string-match ".S$" (buffer-file-name))
    ;; Get the newlines right
    ;; `newline-and-indent' calls `indent-line-function'
    (set (make-local-variable 'indent-line-function) 'indent-relative)
    (define-key asm-mode-map "\C-m" 'newline-and-indent)
    ;; Get the comments right
    (setq comment-column 30)))

; hooks
(add-hook 'desktop-save-hook 'desktop-save-man)
(add-hook 'desktop-save-hook 'desktop-save-rfc)
(add-hook 'asm-mode-set-comment-hook 'arm-asm-mode-set-comment-hook)
(add-hook 'asm-mode-hook 'arm-asm-mode-hook)

;; handle tmux's xterm-keys
;; put the following line in your ~/.tmux.conf:
;;   setw -g xterm-keys on
(if (getenv "TMUX")
    (progn
      (let ((x 2) (tkey ""))
	(while (<= x 8)
	  ;; shift
	  (if (= x 2)
	      (setq tkey "S-"))
	  ;; alt
	  (if (= x 3)
	      (setq tkey "M-"))
	  ;; alt + shift
	  (if (= x 4)
	      (setq tkey "M-S-"))
	  ;; ctrl
	  (if (= x 5)
	      (setq tkey "C-"))
	  ;; ctrl + shift
	  (if (= x 6)
	      (setq tkey "C-S-"))
	  ;; ctrl + alt
	  (if (= x 7)
	      (setq tkey "C-M-"))
	  ;; ctrl + alt + shift
	  (if (= x 8)
	      (setq tkey "C-M-S-"))

	  ;; arrows
	  (define-key key-translation-map (kbd (format "M-[ 1 ; %d A" x))
	    (kbd (format "%s<up>" tkey)))
	  (define-key key-translation-map (kbd (format "M-[ 1 ; %d B" x))
	    (kbd (format "%s<down>" tkey)))
	  (define-key key-translation-map (kbd (format "M-[ 1 ; %d C" x))
	    (kbd (format "%s<right>" tkey)))
	  (define-key key-translation-map (kbd (format "M-[ 1 ; %d D" x))
	    (kbd (format "%s<left>" tkey)))
	  ;; home
	  (define-key key-translation-map (kbd (format "M-[ 1 ; %d H" x))
	    (kbd (format "%s<home>" tkey)))
	  ;; end
	  (define-key key-translation-map (kbd (format "M-[ 1 ; %d F" x))
	    (kbd (format "%s<end>" tkey)))
	  ;; page up
	  (define-key key-translation-map (kbd (format "M-[ 5 ; %d ~" x))
	    (kbd (format "%s<prior>" tkey)))
	  ;; page down
	  (define-key key-translation-map (kbd (format "M-[ 6 ; %d ~" x))
	    (kbd (format "%s<next>" tkey)))
	  ;; insert
	  (define-key key-translation-map (kbd (format "M-[ 2 ; %d ~" x))
	    (kbd (format "%s<delete>" tkey)))
	  ;; delete
	  (define-key key-translation-map (kbd (format "M-[ 3 ; %d ~" x))
	    (kbd (format "%s<delete>" tkey)))
	  ;; f1
	  (define-key key-translation-map (kbd (format "M-[ 1 ; %d P" x))
	    (kbd (format "%s<f1>" tkey)))
	  ;; f2
	  (define-key key-translation-map (kbd (format "M-[ 1 ; %d Q" x))
	    (kbd (format "%s<f2>" tkey)))
	  ;; f3
	  (define-key key-translation-map (kbd (format "M-[ 1 ; %d R" x))
	    (kbd (format "%s<f3>" tkey)))
	  ;; f4
	  (define-key key-translation-map (kbd (format "M-[ 1 ; %d S" x))
	    (kbd (format "%s<f4>" tkey)))
	  ;; f5
	  (define-key key-translation-map (kbd (format "M-[ 15 ; %d ~" x))
	    (kbd (format "%s<f5>" tkey)))
	  ;; f6
	  (define-key key-translation-map (kbd (format "M-[ 17 ; %d ~" x))
	    (kbd (format "%s<f6>" tkey)))
	  ;; f7
	  (define-key key-translation-map (kbd (format "M-[ 18 ; %d ~" x))
	    (kbd (format "%s<f7>" tkey)))
	  ;; f8
	  (define-key key-translation-map (kbd (format "M-[ 19 ; %d ~" x))
	    (kbd (format "%s<f8>" tkey)))
	  ;; f9
	  (define-key key-translation-map (kbd (format "M-[ 20 ; %d ~" x))
	    (kbd (format "%s<f9>" tkey)))
	  ;; f10
	  (define-key key-translation-map (kbd (format "M-[ 21 ; %d ~" x))
	    (kbd (format "%s<f10>" tkey)))
	  ;; f11
	  (define-key key-translation-map (kbd (format "M-[ 23 ; %d ~" x))
	    (kbd (format "%s<f11>" tkey)))
	  ;; f12
	  (define-key key-translation-map (kbd (format "M-[ 24 ; %d ~" x))
	    (kbd (format "%s<f12>" tkey)))
	  ;; f13
	  (define-key key-translation-map (kbd (format "M-[ 25 ; %d ~" x))
	    (kbd (format "%s<f13>" tkey)))
	  ;; f14
	  (define-key key-translation-map (kbd (format "M-[ 26 ; %d ~" x))
	    (kbd (format "%s<f14>" tkey)))
	  ;; f15
	  (define-key key-translation-map (kbd (format "M-[ 28 ; %d ~" x))
	    (kbd (format "%s<f15>" tkey)))
	  ;; f16
	  (define-key key-translation-map (kbd (format "M-[ 29 ; %d ~" x))
	    (kbd (format "%s<f16>" tkey)))
	  ;; f17
	  (define-key key-translation-map (kbd (format "M-[ 31 ; %d ~" x))
	    (kbd (format "%s<f17>" tkey)))
	  ;; f18
	  (define-key key-translation-map (kbd (format "M-[ 32 ; %d ~" x))
	    (kbd (format "%s<f18>" tkey)))
	  ;; f19
	  (define-key key-translation-map (kbd (format "M-[ 33 ; %d ~" x))
	    (kbd (format "%s<f19>" tkey)))
	  ;; f20
	  (define-key key-translation-map (kbd (format "M-[ 34 ; %d ~" x))
	    (kbd (format "%s<f20>" tkey)))

	  (setq x (+ x 1))
	  ))
      )
  )

; key bindings
(global-set-key (kbd "C-k") 'kill-whole-line)
(global-set-key (kbd "M->") 'shifttext-tab-right)
(global-set-key (kbd "M-<") 'shifttext-tab-left)
(global-set-key (kbd "C-c p") 'show-file-path)
(global-set-key (kbd "C-x p") 'other-window-backward)
(global-set-key (kbd "C-c b") 'browse-url-firefox)
(define-key my-minor-mode-map (kbd "DEL") 'backward-delete-whitespace-to-column)
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
(define-key my-minor-mode-map (kbd "C-c s") 'desktop-menu)
(define-key my-minor-mode-map (kbd "C-c v") 'xclip-paste)
(define-key my-minor-mode-map (kbd "C-c c") 'xclip-copy)
(define-key my-minor-mode-map (kbd "C-c x") 'xclip-cut)
(define-key my-minor-mode-map (kbd "C-c m") 'man)
(define-key my-minor-mode-map (kbd "C-c r") 'rfc-index)
(define-key my-minor-mode-map (kbd "C-x C-r") 'recentf-open-files)

(define-minor-mode my-minor-mode
    "A minor mode so that my key settings aren't shadowed by other major/minor modes"
    t "" 'my-minor-mode-map)

(load-file "~/.emacs.d/custom.el")
