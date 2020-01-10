; -*- emacs-lisp -*-

;; Bazel config

(defun my-bazel-mode-hook ()
  (setq tab-width 4
        indent-tabs-mode 1))
(add-hook 'bazel-mode-hook 'my-bazel-mode-hook)

(add-to-list 'auto-mode-alist '("\\.bazel\\'" . bazel-mode))
(add-to-list 'auto-mode-alist '("\\.bzl\\'" . bazel-mode))
(add-to-list 'auto-mode-alist '("WORKSPACE\\'" . bazel-mode))

(use-package bazel-mode)
