; -*- emacs-lisp -*-

(require 'rainbow-delimiters)

(defun jsm/lisp-modes-hook ()
  "Hook this single function to the lisp-like modes"
  (interactive)
  (progn
    (paredit-mode)
    (rainbow-delimiters-mode)
    (git-gutter-mode)))

;; Paredit
(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook                  'jsm/lisp-modes-hook)
(add-hook 'eval-expression-minibuffer-setup-hook 'jsm/lisp-modes-hook)
(add-hook 'ielm-mode-hook                        'jsm/lisp-modes-hook)
(add-hook 'lisp-mode-hook                        'jsm/lisp-modes-hook)
(add-hook 'lisp-interaction-mode-hook            'jsm/lisp-modes-hook)
(add-hook 'scheme-mode-hook                      'jsm/lisp-modes-hook)
(add-hook 'geiser-repl-mode-hook                 'jsm/lisp-modes-hook)
(add-hook 'clojure-mode-hook                     'jsm/lisp-modes-hook)

;; Scheme Development
;; ------------------------------
;; Geiser - Scheme development environment
(load-file "~/.emacs.d/geiser/elisp/geiser.el")
; Leaving out guile for now
(setq geiser-active-implementations '(racket))
(setq geiser-repl-query-on-kill-p nil
      geiser-repl-query-on-exit-p nil
      geiser-repl-startup-time 1000)
; (setq geiser-racket--prompt-regexp "\\(mzscheme\\|racket\\)@[^ ]*> ") ; Original
(setq geiser-repl-history-filename "~/.emacs.d/geiser-history")

;; Clojure Development - aka Cider config
;; https://github.com/clojure-emacs/cider
;; ------------------------------
(eval-after-load "cider"
  '(progn (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
          (add-hook 'cider-repl-mode-hook 'jsm/lisp-modes-hook)
          (setq cider-repl-result-prefix ";; => ")))
