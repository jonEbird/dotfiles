; -*- emacs-lisp -*-

; Jedi auto completion support
(if (require 'jedi nil 'noerror)
  ; (add-hook 'python-mode-hook 'auto-complete-mode 'append)
  (add-hook 'python-mode-hook 'jedi:setup)
  (setq jedi:setup-keys t
        jedi:complete-on-dot t))

(defun my-python-mode-hook ()
  "Minor setup when entering Python mode"
  (interactive)
  (hs-minor-mode)
  (flyspell-prog-mode))

(add-hook 'python-mode-hook 'my-python-mode-hook)

;; enable flycheck support
;; Would have tried Steve Purcell's flymake-python-pyflakes had flycheck
;; not worked out for me.

(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)
(setq flycheck-idle-change-delay 15)

(cond ((boundp 'helm-mode)
       (setq flycheck-completion-system 'nil))
      ((boundp 'ido-mode)
       (setq flycheck-completion-system 'ido)))

;; concider customizing Variables:
;;  flycheck-flake8-error-level-alist
;;  flycheck-flake8-maximum-complexity
;;  flycheck-flake8-maximum-line-length
;;  flycheck-flake8rc -> ~/.config/flake8
