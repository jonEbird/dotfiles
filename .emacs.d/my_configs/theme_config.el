;;; package --- Summary

;;; Commentary:

(require 'color-theme)
;; (require 'smart-mode-line)

;; Require my theme(s)
(require 'color-theme-solarized)

;;; Code:

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :config (display-time-mode -1))

;; ;; Smart-mode-line
;; ;; ------------------------------
;; (require 'smart-mode-line)
;; (setq my-dark-theme       'solarized-dark
;;       my-dark-theme-sml   'respectful
;;       my-light-theme      'solarized-light
;;       my-light-theme-sml  'automatic)

;; (color-theme-solarized)

;; (defun toggle-night-color-theme ()
;;   "Switch to/from night color scheme, including shell theme, for presentation mode."
;;   (interactive)
;;   (if (eq (frame-parameter (next-frame) 'background-mode) 'light)
;;       (progn
;;         (load-theme my-dark-theme nil nil)
;;         (if (eq system-type 'gnu/linux)
;;             (shell-command "~/gnome-terminal-colors-solarized/install.sh -s dark -p default" nil nil))
;;         (setq sml/theme my-dark-theme-sml))
;;     (load-theme my-light-theme nil nil)
;;     (if (eq system-type 'gnu/linux)
;;         (shell-command "~/gnome-terminal-colors-solarized/install.sh -s light -p default" nil nil))
;;     (setq sml/theme my-light-theme-sml))
;;   (sml/setup))

;; ;; Toggle between light and dark themes with F7
;; (global-set-key (kbd "<f7>") 'toggle-night-color-theme)

;; (setq sml/theme           my-dark-theme-sml
;;       sml/shorten-modes   t
;;       sml/mode-width      'full
;;       sml/name-width      25
;;       sml/hidden-modes    '(" hl-p" " Undo-Tree" " Guide" " pair" " ARev" " GitGutter" " Helm"
;;                             " Paredit" " fs" " ElDoc" " WS" " Fly" " Abbrev" " Gtags" " HelmGtags" " MRev"
;;                             " Google" " Wrap" " AC" " ivy"))
;; (add-to-list 'sml/replacer-regexp-list '("^/repos/" ":Repo:"))
;; (sml/setup)

;; Extra customization for look and feel
(set-face-attribute 'org-code nil :foreground "forest green")
(set-face-attribute 'org-block nil :foreground "forest green")


;; https://github.com/hlissner/emacs-doom-themes
(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t       ; if nil, bold is universally disabled
        doom-themes-enable-italic t)    ; if nil, italics is universally disabled
  (load-theme 'doom-one t)
  ;; (load-theme 'doom-city-lights t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)

  ;; increase font and frame size (default is 120) - See (read-face-attribute 'default :height)
  (set-face-attribute 'default nil :height 130)

  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
  (doom-themes-treemacs-config)

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(provide 'theme_config)
;;; theme_config.el ends here
