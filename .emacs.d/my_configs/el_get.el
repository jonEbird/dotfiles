;; El-Get Support

; Basic Setup
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")

; Establish that some packages come from ELPA, etc.
(setq
 el-get-sources
 '(el-get                   ; self-hosting
   (:name cups
          :type elpa)
   (:name cl-lib
          :type elpa)
   (:name magit
          :type elpa
          :after (progn ()
		   (global-set-key (kbd "C-x C-z") 'magit-status)))
   (:name org2blog
          :type elpa)
   ))

; (el-get 'sync)

; Finally my list of packages wanting to be installed
(setq jsm-packages
      (append
       '(cups ace-jump-mode yaml-mode color-theme color-theme-solarized lua-mode
              etags-select etags-table markdown-mode cups cl-lib magit htmlize
              org2blog)
       
       (mapcar 'el-get-as-symbol (mapcar 'el-get-source-name el-get-sources))))

(el-get 'sync jsm-packages)
