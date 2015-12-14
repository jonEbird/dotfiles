; -*- emacs-lisp -*-

;;; Windows Configuration
;;;   Setting up settings only for windows.
;;;   For anything non-trivial, create a function first,
;;;    and then call it within the final when-block below.
;;; FIXME - Find a better way to do this

;; --------------------------------------------------
;; Non-trivial Setups
;; --------------------------------------------------

; Login information
(require 'netrc)

;; confluence support
; Downloaded and extracted from http://confluence-el.googlecode.com/files/confluence-el-1.5.tar.gz
(defun confluence-support-setup()
  ; (setq load-path (cons (expand-file-name "~/.emacs.d/confluence-el/") load-path))
  (require 'confluence)
  (custom-set-variables
   '(confluence-url "https://confluence.qualcomm.com/confluence/rpc/xmlrpc")
   '(confluence-default-space-alist (list (cons confluence-url "MOAB")))
   '(confluence-creds (netrc-machine (netrc-parse "~/.netrc.gpg") "confluence" t))
   '(confluence-save-credentials t)
   '(confluence-login-credential-alist (list (cons confluence-url (cons (netrc-get confluence-creds "login") (netrc-get confluence-creds "password")))))
   )
  (eval-after-load "confluence"
    '(progn
       (require 'longlines)
       (progn
	 (add-hook 'confluence-mode-hook 'longlines-mode)
	 (add-hook 'confluence-mode-hook 'flyspell-mode)
	 (add-hook 'confluence-before-save-hook 'longlines-before-revert-hook)
	 (add-hook 'confluence-before-revert-hook 'longlines-before-revert-hook)
	 (add-hook 'confluence-mode-hook '(lambda () (local-set-key "\C-j" 'confluence-newline-and-indent))))))
  (global-set-key "\C-xwf" 'confluence-get-page)
  (defalias 'wiki-get-page 'confluence-get-page)
  (add-hook 'confluence-mode-hook
	    '(lambda ()
	       (local-set-key "\C-xw" confluence-prefix-map)))
  ;; (setq tls-program
  ;;       '("gnutls-cli --x509cafile /etc/pki/tls/certs/ca-bundle.crt -p %p %h"
  ;;         "gnutls-cli --x509cafile /etc/pki/tls/certs/ca-bundle.crt -p %p %h --protocols ssl3"
  ;;         "openssl s_client -connect %h:%p -CAfile /etc/pki/tls/certs/ca-bundle.crt -no_ssl2 -ign_eof"))
  (setq tls-program
        '("openssl s_client -connect %h:%p -no_ssl2 -ign_eof"))
          ;; "gnutls-cli -p %p %h"
          ;; "gnutls-cli -p %p %h --protocols ssl3"
  )

;; --------------------------------------------------
;; Now actually set the variables and more complicated setups
;; --------------------------------------------------
(when (eq system-type 'windows-nt)
  (message "Setting up specific settings for Winblows")
  (setq
   ispell-program-name "C:\\Users\\jsmiller\\software\\Aspell\\bin\\aspell.exe"
   ; "c:\\Program Files (x86)\\Aspell\\bin\\aspell.exe"
   ;; url-proxy-services '(("no_proxy" . "nwie\\.net")
   ;; 			("http" . "http-proxy.nwie.net:8080"))
   )
  (setq magit-git-executable "C:\\Program Files (x86)\\Git\\bin\\git.exe")
  (confluence-support-setup)
  (setq org-file-apps '((auto-mode . emacs)
			("\\.mm\\'" . default)
			("\\.x?html?\\'" . default)
			("\\.pdf\\'" . default)
			("\\.xlsx?" . default)
			("\\.docx?" . default)))
  ;; Use Putty tools for remote editing
  (setq tramp-default-method "pscp")
  (load-theme 'deeper-blue nil nil)
  ;; Thank you - http://stackoverflow.com/questions/5436563/setting-emacs-font-under-windows
  (set-face-attribute 'default nil
		      :family "Consolas" :height 100)
  )
