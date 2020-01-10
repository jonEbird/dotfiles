; -*- emacs-lisp -*-

;; Essentially taken from https://github.com/cockroachdb/cockroach/wiki/Ben's-Go-Emacs-setup
;; && http://tleyden.github.io/blog/2014/05/22/configure-emacs-as-a-go-editor-from-scratch/

;; Go installs needed:
;; $ go get golang.org/x/tools/cmd/goimports
;; $ go get -u github.com/nsf/gocode
;; $ go get golang.org/x/tools/cmd/goimports

;; For a lsp setup, consider:
;; https://arenzana.org/2019/12/emacs-go-mode-revisited/

;; For lsp, need to install this, and possibly keep it up to date:
;; GO111MODULE=on go get golang.org/x/tools/gopls@latest

(defun my-go-mode-hook ()
  (setq tab-width 4
        indent-tabs-mode 1)
  (hs-minor-mode)
  (flyspell-prog-mode)
  (flycheck-mode 1)
  (highlight-thing-mode)
  (flycheck-golangci-lint-setup)
  ;; eldoc shows the signature of the function at point in the status bar.
  (go-eldoc-setup)
  ;; (local-set-key (kbd "M-.") #'godef-jump)
  (add-hook 'before-save-hook 'gofmt-before-save)
  ;; Setup company for completion
  (company-mode)
  (auto-complete-mode 1)
  (set (make-local-variable 'company-backends) '(company-go)))
(add-hook 'go-mode-hook 'my-go-mode-hook)

;; Some of these are from gotest
;; gotest will use gb (https://getgb.io/) if installed

(use-package go-mode
  :bind (:map go-mode-map
              ("M-." . godef-jump)
              ("M-*" . pop-tag-mark)))

(use-package gotest
  :bind (("C-c a" . go-test-current-project)
         ("C-c m" . go-test-current-file)
         ("C-c ." . go-test-current-test)
         ("C-c b" . go-run)))

;; This is for generating tests
(use-package gotests)

(use-package go-projectile
  :custom ((gofmt-command (concat go-projectile-tools-path "/bin/goimports")))
  :config (go-projectile-tools-add-path))

(use-package company-go)

;; brew install golangci/tap/golangci-lint
;; -Or- go get -u github.com/golangci/golangci-lint/cmd/golangci-lint
(use-package flycheck-golangci-lint)
