; -*- emacs-lisp -*-

;; Elixir Language
;; ------------------------------
(defalias 'elixir-repl 'elixir-mode-iex "Alias to the elixir iex REPL")

;; Great tool for showing where else in the buffer the thing-at-point is located
(require 'highlight-thing)
(custom-set-faces
 '(highlight-thing ((t (:inherit 'underline)))))

(require 'flycheck)
(setq flycheck-shellcheck-excluded-warnings '("SC2086"))

;; (require 'thrift)

;; bb-mode for bitbake recipes
(require 'bb-mode)
(setq auto-mode-alist (cons '("\\.bb$" . bb-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.inc$" . bb-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.bbappend$" . bb-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.bbclass$" . bb-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.conf$" . bb-mode) auto-mode-alist))

;; Enable flycheck for comments in other languages
(dolist (hook
         '(c-mode-hook c++-mode-hook python-mode-hook
                       emacs-lisp-mode-hook shell-mode-hook
                       prog-mode-hook))
  (add-hook hook 'flyspell-prog-mode 'append))
