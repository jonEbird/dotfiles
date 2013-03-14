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
  (setq browse-url-generic-program "google-chrome"
	browse-url-browser-function 'browse-url-generic)
  ; FIXME
  (setq org-file-apps '((auto-mode . emacs)
		      ("\\.mm\\'" . default)
		      ("\\.x?html?\\'" . "/opt/google/chrome/google-chrome %s")
		      ("\\.pdf\\'" . default)) )
  ;; (load-theme 'tango-dark nil nil)
  ;; (eval-after-load "magit"
  ;;   '(set-face-attribute 'magit-item-highlight nil :foreground "#ffffff" :background "#3f4747"))
  (load-theme 'solarized-dark nil nil)
  )
