(setq my-packages '(irony-mode ggtags go-mode arduino-mode rainbow-mode))

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (let (el-get-install-skip-emacswiki-recipes)
	(goto-char (point-max))
	(eval-print-last-sexp))))

(el-get 'sync my-packages)
