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
