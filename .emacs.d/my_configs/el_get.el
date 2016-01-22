;; El-Get Support

; Basic Setup
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

;; (unless (require 'el-get nil 'noerror)
;;   (with-current-buffer
;;       (url-retrieve-synchronously
;;        "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
;;     (goto-char (point-max))
;;     (eval-print-last-sexp)))

;; Use the master branch
(unless (require 'el-get nil 'noerror)
  (url-retrieve
   "https://raw.github.com/dimitri/el-get/master/el-get-install.el"
   (lambda (s)
     (let (el-get-master-branch)
       (goto-char (point-max))
       (eval-print-last-sexp)))))

(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")

; Establish that some packages come from ELPA, etc.
(setq
 el-get-sources
 '(el-get                   ; self-hosting
   (:name cups
    :type elpa)
   (:name etags-table
    :type elpa)
   (:name xml-rpc
    :type elpa)
   (:name org2blog
    :type elpa)
   (:name org-mime
    :type elpa)
   (:name guide-key
    :type elpa)
   ))

; (el-get 'sync)

;; Special quick hack for Mac platform before getting too far
(when (eq system-type 'darwin)
  (el-get 'sync '(exec-path-from-shell))
  (exec-path-from-shell-copy-env "PATH"))

; Finally my list of packages wanting to be installed
(setq jsm-packages
      (append
       (mapcar 'el-get-as-symbol (mapcar 'el-get-source-name el-get-sources))

       '(cups ace-jump-mode yaml-mode color-theme color-theme-solarized ; names
         lua-mode markdown-mode google-maps htmlize ldap-mode url-dav
         org-caldav confluence-el expand-region iedit jedi mu4e
         flx paredit ido-vertical-mode dash ; php-mode-improved
         multiple-cursors popwin guide-key visual-regexp projectile
         ack-and-a-half cl-lib autopair yasnippet auto-complete-clang
         gtags multiple-cursors magit git-gutter
         idomenu undo-tree org-mode cmake-mode gist session smart-mode-line
         rpm-spec-mode cider rainbow-delimiters elixir org-reveal
         graphviz-dot-mode term+ term+ki term+mux phi-search smex
         helm helm-git-grep helm-mu color-theme-zenburn dockerfile-mode
         helm-gtags command-log-mode helm-swoop key-chord flycheck
         ag helm-ag wdired org-bullets zeal-at-point org-download
         ace-window avy hydra use-package bb-mode swiper docker-tramp)))
; term+ term+ki term+mux faux-screen
; auto-complete-etags elip powerline

(el-get 'sync jsm-packages)
