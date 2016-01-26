;;; package --- Summary

;;; Commentary:

(require 'color-theme)
(require 'smart-mode-line)

;; Require my theme(s)
(require 'color-theme-solarized)

;;; Code:

;; Smart-mode-line
;; ------------------------------
(require 'smart-mode-line)
(setq my-dark-theme       'solarized-dark
      my-dark-theme-sml   'respectful
      my-light-theme      'solarized-light
      my-light-theme-sml  'automatic)

(color-theme-solarized)

(defun toggle-night-color-theme ()
  "Switch to/from night color scheme, including shell theme, for presentation mode."
  (interactive)
  (if (eq (frame-parameter (next-frame) 'background-mode) 'light)
      (progn
        (load-theme my-dark-theme nil nil)
        (if (eq system-type 'gnu/linux)
            (shell-command "~/gnome-terminal-colors-solarized/install.sh -s dark -p default" nil nil))
        (setq sml/theme my-dark-theme-sml))
    (load-theme my-light-theme nil nil)
    (if (eq system-type 'gnu/linux)
        (shell-command "~/gnome-terminal-colors-solarized/install.sh -s light -p default" nil nil))
    (setq sml/theme my-light-theme-sml))
  (sml/setup))

;; Toggle between light and dark themes with F7
(global-set-key (kbd "<f7>") 'toggle-night-color-theme)

(setq sml/theme           my-dark-theme-sml
      sml/shorten-modes   t
      sml/mode-width      'full
      sml/name-width      25
      sml/hidden-modes    '(" hl-p" " Undo-Tree" " Guide" " pair" " ARev" " GitGutter" " Helm"
                            " Paredit" " fs" " ElDoc" " WS" " Fly" " Abbrev" " Gtags" " HelmGtags" " MRev"))
(add-to-list 'sml/replacer-regexp-list '("^/repos/" ":Repo:"))
(sml/setup)

(provide 'theme_config)
;;; theme_config.el ends here
