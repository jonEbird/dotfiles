;;; package --- Summary

;;; Commentary:

;; Configure development for Ruby
;; Much inspiration taken from:
;;   http://crypt.codemancers.com/posts/2013-09-26-setting-up-emacs-as-development-environment-on-osx/

;;; Code:

;; Auto-complete
(require 'auto-complete-config)
(ac-config-default)
(setq ac-ignore-case nil)
;; (add-to-list 'ac-modes 'enh-ruby-mode)
(add-to-list 'ac-modes 'ruby-mode)
(add-to-list 'ac-modes 'web-mode)

;; Ruby Mode for all the Ruby related files
;; (require 'enh-ruby-mode)
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . ruby-mode))

;; Web Mode for erb files
(use-package web-mode
  :mode "\\.erb$")

;; Ruby Manager - I went with rbenv

;; (if (eq system-type 'darwin)
;;     (setq rbenv-installation-dir "/opt/homebrew"
;;           ;(replace-regexp-in-string "\n$" "" (shell-command-to-string "brew --prefix rbenv"))
;;           ))
(use-package rbenv
  :custom ((rbenv-show-active-ruby-in-modeline nil)
           (ruby-deep-indent-paren nil))
  :config (global-rbenv-mode))

;; Inf Ruby
(use-package inf-ruby)

;; Ruby tools
(use-package ruby-tools)

;; Yari is great for method lookups
(use-package yari)

;; Also yard (gem install yard) is useful too but I haven't played around with it yet
; (use-package yard-mode)

;; cucumber work
(use-package feature-mode)

(defun my-feature-mode-hook ()
  (highlight-thing-mode)
  (rainbow-delimiters-mode))

(add-hook 'feature-mode-hook 'my-feature-mode-hook)

;; robe mode configuration
(use-package robe
  :hook (robe-mode-hook . ac-robe-setup))

(defun my-ruby-mode-hook ()
  "Minor setup when entering Ruby mode."
  (interactive)
  (robe-mode)
  (hs-minor-mode)
  (highlight-thing-mode)
  (ac-flyspell-workaround)
  (flyspell-prog-mode)
  (flycheck-mode 1)
  (rainbow-delimiters-mode)
  (inf-ruby-minor-mode +1)
  (ruby-tools-mode +1)
  (setq ruby-indent-level 4)
  (define-key 'help-command (kbd "R") 'yari)
  ;; These linters are annoying to configure
  ;; (add-to-list 'flycheck-disabled-checkers 'ruby-rubocop)
  (add-to-list 'flycheck-disabled-checkers 'ruby-rubylint))

(add-hook 'ruby-mode-hook 'my-ruby-mode-hook)
;; (add-hook 'enh-ruby-mode-hook 'my-ruby-mode-hook)

(provide 'ruby_config)
;;; ruby_config.el ends here
