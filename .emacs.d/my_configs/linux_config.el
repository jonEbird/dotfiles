; -*- emacs-lisp -*-

;;; Linux Configuration

(defun toggle-night-color-theme ()
  "Switch to/from night color scheme, including shell theme, for presentation mode"
  (interactive)
  (require 'color-theme)
  (if (eq (frame-parameter (next-frame) 'background-mode) 'light)
      (progn
        (load-theme 'solarized-dark nil nil)
        (shell-command "~/gnome-terminal-colors-solarized/set_dark.sh" nil nil))
    (load-theme 'solarized-light nil nil)
    (shell-command "~/gnome-terminal-colors-solarized/set_light.sh" nil nil)))

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
  ; Toggle between light and dark themes with F7
  (global-set-key (kbd "<f7>") 'toggle-night-color-theme)
  )
