; -*- emacs-lisp -*-

;; Elixir Language
;; ------------------------------
(defalias 'elixir-repl 'elixir-mode-iex "Alias to the elixir iex REPL")

(require 'flycheck)
(setq flycheck-shellcheck-excluded-warnings '("SC2086"))

(require 'thrift)
