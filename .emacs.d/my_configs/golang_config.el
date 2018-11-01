; -*- emacs-lisp -*-

;; Essentially taken from https://github.com/cockroachdb/cockroach/wiki/Ben's-Go-Emacs-setup

;; Go installs needed:
;; $ go get golang.org/x/tools/cmd/goimports

(defun my-go-mode-hook ()
  (setq tab-width 4
        indent-tabs-mode 1)
  (hs-minor-mode)
  (flyspell-prog-mode)
  (flycheck-mode 1)
  ;; eldoc shows the signature of the function at point in the status bar.
  (go-eldoc-setup)
  (local-set-key (kbd "M-.") #'godef-jump)
  (add-hook 'before-save-hook 'gofmt-before-save)
  ;; Setup company for completion
  (company-mode)
  (set (make-local-variable 'company-backends) '(company-go)))
(add-hook 'go-mode-hook 'my-go-mode-hook)

;; Some of these are from gotest
;; gotest will use gb (https://getgb.io/) if installed

(use-package go-mode
  :bind (("M-." . godef-jump))
  )

(use-package gotest
  :bind (("C-c a" . go-test-current-project)
         ("C-c m" . go-test-current-file)
         ("C-c ." . go-test-current-test)
         ("C-c b" . go-run)))

;; This is for generating tests
(use-package gotests
  :config (progn
            (dolist (elt go-test-compilation-error-regexp-alist-alist)
              (add-to-list 'compilation-error-regexp-alist-alist elt))
            (dolist (elt (reverse go-test-compilation-error-regexp-alist))
              (add-to-list 'compilation-error-regexp-alist elt t))))

(use-package go-projectile
  :custom ((gofmt-command (concat go-projectile-tools-path "/bin/goimports")))
  :config (go-projectile-tools-add-path))

(use-package company-go)

(use-package flycheck-gometalinter
  :custom ((flycheck-gometalinter-vendor t)
           (flycheck-gometalinter-test t))
  :config (flycheck-gometalinter-setup))
