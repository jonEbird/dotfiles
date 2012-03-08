; -*- emacs-lisp -*-

;;; Windows Configuration
;;;   Setting up settings only for windows.
;;;   For anything non-trivial, create a function first,
;;;    and then call it within the final when-block below.
;;; FIXME - Find a better way to do this

;; --------------------------------------------------
;; Non-trivial Setups
;; --------------------------------------------------

;; confluence support
; Downloaded and extracted from http://confluence-el.googlecode.com/files/confluence-el-1.5.tar.gz
(defun confluence-support-setup()
  (setq load-path (cons (expand-file-name "~/.emacs.d/confluence-el/") load-path))
  (require 'confluence)
  (setq confluence-url "http://nw-wiki.nwie.net/wiki/rpc/xmlrpc"
	confluence-default-space-alist (list (cons confluence-url "virtual")))
  (eval-after-load "confluence"
    '(progn
       (require 'longlines)
       (progn
	 (add-hook 'confluence-mode-hook 'longlines-mode)
	 (add-hook 'confluence-before-save-hook 'longlines-before-revert-hook)
	 (add-hook 'confluence-before-revert-hook 'longlines-before-revert-hook)
	 (add-hook 'confluence-mode-hook '(lambda () (local-set-key "\C-j" 'confluence-newline-and-indent))))))
  (global-set-key "\C-xwf" 'confluence-get-page)
  (add-hook 'confluence-mode-hook
	    '(lambda ()
	       (local-set-key "\C-xw" confluence-prefix-map)))
  ; Login information
  (require 'netrc)
  (setq confluence-creds (netrc-machine (netrc-parse "~/.netrc") "confluence" t))
  (setq confluence-save-credentials t
	confluence-login-credential-alist
	(cons confluence-url (list (cons (netrc-get confluence-creds "login") (netrc-get confluence-creds "password")))))
  )

;; --------------------------------------------------
;; Now actually set the variables and more complicated setups
;; --------------------------------------------------
(when (eq system-type 'windows-nt)
  (message "Setting up specific settings for Winblows")
  (setq
   ispell-program-name "c:\\Program Files\\Aspell\\bin\\aspell.exe"
   ;; url-proxy-services '(("no_proxy" . "nwie\\.net")
   ;; 			("http" . "http-proxy.nwie.net:8080"))
   )
  (confluence-support-setup)
  )
