(setq my-packages '(ggtags go-mode arduino-mode rainbow-mode markdown-mode company-ycmd))

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (let (el-get-install-skip-emacswiki-recipes)
	(goto-char (point-max))
	(eval-print-last-sexp))))

(require 'package)
(add-to-list 'package-archives '("ELPA" . "http://tromey.com/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

(el-get-elpa-build-local-recipes)
(el-get 'sync my-packages)
