; -*- emacs-lisp -*-

;;; Linux Configuration

;; --------------------------------------------------
;; Non-trivial Setups
;; --------------------------------------------------

;; Thanks URLs
;; http://www.emacswiki.org/emacs/BrowseUrl

;; --------------------------------------------------
;; Now actually set the variables and more complicated setups
;; --------------------------------------------------
(when (eq system-type 'gnu/linux)
  (message "Setting up specific settings for Linux")
  (setq browse-url-generic-program
	(substring (shell-command-to-string "gconftool-2 -g /desktop/gnome/applications/browser/exec") 0 -1)
	browse-url-browser-function 'browse-url-generic)
  ; FIXME
  (setq org-file-apps '((auto-mode . emacs)
		      ("\\.mm\\'" . default)
		      ("\\.x?html?\\'" . "/opt/google/chrome/google-chrome %s")
		      ("\\.pdf\\'" . default)) )
  )
