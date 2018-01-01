; -*- emacs-lisp -*-

;; ; Jedi auto completion support
;; (if (require 'jedi nil 'noerror)
;;   ; (add-hook 'python-mode-hook 'auto-complete-mode 'append)
;;   (add-hook 'python-mode-hook 'jedi:setup)
;;   (setq jedi:setup-keys t
;;         jedi:complete-on-dot t))

(use-package jedi
  :custom ((jedi:setup-keys t)
           (jedi:complete-on-dot t))
  :hook (python-mode . jedi:setup))

(defun my-python-mode-hook ()
  "Minor setup when entering Python mode."
  (interactive)
  (hs-minor-mode)
  (flyspell-prog-mode)
  (highlight-thing-mode)
  (ac-flyspell-workaround))

(add-hook 'python-mode-hook 'my-python-mode-hook)

;; Would have tried Steve Purcell's flymake-python-pyflakes had flycheck
;; not worked out for me.

;; consider customizing Variables:
;;  flycheck-flake8-error-level-alist
;;  flycheck-flake8-maximum-complexity
;;  flycheck-flake8-maximum-line-length
;;  flycheck-flake8rc -> ~/.config/flake8
