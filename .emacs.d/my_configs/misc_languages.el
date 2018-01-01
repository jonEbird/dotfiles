; -*- emacs-lisp -*-

;; Elixir Language
;; ------------------------------
(defalias 'elixir-repl 'elixir-mode-iex "Alias to the elixir iex REPL")

;; Great tool for showing where else in the buffer the thing-at-point is located
(use-package highlight-thing
  :custom-face (highlight-thing ((t (:inherit 'underline)))))
; (require 'highlight-thing)
;; (custom-set-faces
;;  '(highlight-thing ((t (:inherit 'underline)))))

(use-package flycheck
  :custom ((flycheck-shellcheck-excluded-warnings '("SC2086"))
           (flycheck-idle-change-delay 15))
  :hook (c-mode-hook c++-mode-hook python-mode-hook
                     emacs-lisp-mode-hook shell-mode-hook
                     prog-mode-hook)
  :config (cond ((boundp 'helm-mode)
                 (setq flycheck-completion-system 'nil))
                ((boundp 'ido-mode)
                 (setq flycheck-completion-system 'ido))))

;; (require 'thrift)

;; bb-mode for bitbake recipes
(use-package bb-mode
  :mode (("\\.bb$" . bb-mode)
         ("\\.inc$" . bb-mode)
         ("\\.bbappend$" . bb-mode)
         ("\\.bbclass$" . bb-mode)
         ("\\.conf$" . bb-mode)))

;; Javascript
(setq js-indent-level 2)

;; yaml
(use-package flycheck-yamllint
  :hook (flycheck-mode . flycheck-yamllint-setup))
