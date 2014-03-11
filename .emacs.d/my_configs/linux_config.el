; -*- emacs-lisp -*-

;;; Linux Configuration
(require 'color-theme)
(require 'smart-mode-line)

(defun toggle-night-color-theme ()
  "Switch to/from night color scheme, including shell theme, for presentation mode"
  (interactive)
  (if (eq (frame-parameter (next-frame) 'background-mode) 'light)
      (progn
        (load-theme 'solarized-dark nil nil)
        (shell-command "~/gnome-terminal-colors-solarized/install.sh -s dark -p default" nil nil)
        (setq sml/theme 'respectful))
    (load-theme 'solarized-light nil nil)
    (shell-command "~/gnome-terminal-colors-solarized/install.sh -s light -p default" nil nil)
    (setq sml/theme 'light))
  (sml/setup))

(when (eq system-type 'gnu/linux)
  (message "Setting up specific settings for Linux")
  (setq browse-url-generic-program "google-chrome"
	browse-url-browser-function 'browse-url-generic)
  ; FIXME
  (setq org-file-apps '((auto-mode . emacs)
		      ("\\.mm\\'" . default)
		      ("\\.x?html?\\'" . "/opt/google/chrome/google-chrome %s")
		      ("\\.pdf\\'" . "/usr/bin/evince %s")) )
  ;; (load-theme 'tango-dark nil nil)
  ;; (eval-after-load "magit"
  ;;   '(set-face-attribute 'magit-item-highlight nil :foreground "#ffffff" :background "#3f4747"))
  (load-theme 'solarized-dark nil nil)
  ; Toggle between light and dark themes with F7
  (global-set-key (kbd "<f7>") 'toggle-night-color-theme)

  ;; Printing support!
  (require 'printing)
  (pr-update-menus t)
  ; make sure we use localhost as cups server
  (setenv "CUPS_SERVER" "localhost")
  (require 'cups)

  ;; Install yaml-mode via the ELPA repository and associate file types
  (require 'yaml-mode)
  (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
  (add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))

  )
