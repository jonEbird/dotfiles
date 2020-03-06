; -*- emacs-lisp -*-

;; Elixir Language
;; ------------------------------
(defalias 'elixir-repl 'elixir-mode-iex "Alias to the elixir iex REPL")

;; Taken from phi-search (phi-search-match-face)
(defface my-highlight-face
  '((((background light)) (:background "#b5dee9"))
    (t (:background "#194854")))
  "Face used to highlight things.")

;; Great tool for showing where else in the buffer the thing-at-point is located
(use-package highlight-thing
  :custom ((highlight-thing-case-sensitive-p nil)
           (highlight-thing-ignore-list '("False" "True" "import" "private" "public" "final" "return"))
           (highlight-thing-exclude-thing-under-point t))
  :custom-face (highlight-thing ((t (:inherit 'my-highlight-face)))))


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

(use-package dap-mode
  :diminish
  ;; :bind
  ;; (:map dap-mode-map
  ;;       (("<f12>" . dap-debug)
  ;;        ("<f8>" . dap-continue)
  ;;        ("<f9>" . dap-next)
  ;;        ("<M-f11>" . dap-step-in)
  ;;        ("C-M-<f11>" . dap-step-out)
  ;;        ("<f7>" . dap-breakpoint-toggle)))
  :config
  ;; (dap-mode 1) ;; these two lines are enabled via first two hooks below
  ;; (dap-ui-mode 1)
  ;; enables mouse hover support
  (dap-tooltip-mode 1)
  ;; use tooltips for mouse hover
  ;; if it is not enabled `dap-mode' will use the minibuffer.
  (tooltip-mode 1)
  :hook ((after-init . dap-mode)
         (dap-mode . dap-ui-mode)
         (python-mode . (lambda () (require 'dap-python)))
         (ruby-mode . (lambda () (require 'dap-ruby)))
         (go-mode . (lambda () (require 'dap-go)))
         (java-mode . (lambda () (require 'dap-java)))
         ((c-mode c++-mode objc-mode swift) . (lambda () (require 'dap-lldb)))
         (php-mode . (lambda () (require 'dap-php)))
         (elixir-mode . (lambda () (require 'dap-elixir)))
         ((js-mode js2-mode typescript-mode) . (lambda () (require 'dap-chrome)))))

;; (require 'thrift)

;; bb-mode for bitbake recipes
(use-package bb-mode
  :mode (("\\.bb$" . bb-mode)
         ("\\.inc$" . bb-mode)
         ("\\.bbappend$" . bb-mode)
         ("\\.bbclass$" . bb-mode)
         ("\\.conf$" . bb-mode)))

;; yaml
(use-package flycheck-yamllint
  :hook (flycheck-mode . flycheck-yamllint-setup))
