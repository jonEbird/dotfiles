; -*- emacs-lisp -*-

;; ; Jedi auto completion support
;; (if (require 'jedi nil 'noerror)
;;   ; (add-hook 'python-mode-hook 'auto-complete-mode 'append)
;;   (add-hook 'python-mode-hook 'jedi:setup)
;;   (setq jedi:setup-keys t
;;         jedi:complete-on-dot t))

;; python-mode-map

(use-package jedi
  :after company
  :custom ((jedi:setup-keys t)
           (jedi:complete-on-dot t))
  :hook (python-mode . jedi:setup)
  :bind (:map python-mode-map
              ("M-." . jedi:goto-definition)
              ("M-*" . jedi:goto-definition-pop-marker)
              ("M-," . jedi:goto-definition-pop-marker))
  :config
  (add-to-list 'company-backends 'company-jedi))

;; jedi:goto-definition-config  "M-."
;; jedi:key-goto-definition-pop-marker "M-*"  jedi-mode-map

(setq python--prettify-symbols-alist
      '(("import pdb; pdb.set_trace()" . "🛑")
        ("lambda" . 955)))

(defun jsm:insert-python-breakpoint ()
  "Insert typical Python breakpoint code."
  (interactive)
  (insert "import pdb; pdb.set_trace()"))

(defun my-python-mode-hook ()
  "Minor setup when entering Python mode."
  (interactive)
  (direnv-mode)
  (hs-minor-mode)
  (flyspell-prog-mode)
  (flycheck-mode 1)
  ;; (flycheck-select-checker 'python-pylint)  ;; use "C-c ! s" to select alt checker
  (ignore-errors
    (flycheck-select-checker 'python-flake8))
  (git-gutter-mode 1)
  (highlight-thing-mode)
  (prettify-symbols-mode)
  (setq fill-column 90)
  (ac-flyspell-workaround))

(use-package python
  :after direnv
  :bind (:map python-mode-map
              ("M-s s" . jsm:insert-python-breakpoint))
  :custom (flycheck-python-pylint-executable
           (concat "python " (locate-file "pylint" exec-path)))
  :init
  (add-hook 'python-mode-hook 'my-python-mode-hook))

;; Would have tried Steve Purcell's flymake-python-pyflakes had flycheck
;; not worked out for me.

;; consider customizing Variables:
;;  flycheck-flake8-error-level-alist
;;  flycheck-flake8-maximum-complexity
;;  flycheck-flake8-maximum-line-length
;;  flycheck-flake8rc -> ~/.config/flake8
