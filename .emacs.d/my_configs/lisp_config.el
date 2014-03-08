; -*- emacs-lisp -*-

;; Paredit
(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook 'enable-paredit-mode)
(add-hook 'ielm-mode-hook             'enable-paredit-mode)
(add-hook 'lisp-mode-hook             'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)
(add-hook 'scheme-mode-hook           'enable-paredit-mode)
(add-hook 'geiser-repl-mode-hook      'enable-paredit-mode)

;; Geiser - Scheme development environment
(load-file "~/.emacs.d/geiser/elisp/geiser.el")
; Leaving out guile for now
(setq geiser-active-implementations '(racket))
(setq geiser-repl-query-on-kill-p nil
      geiser-repl-query-on-exit-p nil
      geiser-repl-startup-time 1000)
; (setq geiser-racket--prompt-regexp "\\(mzscheme\\|racket\\)@[^ ]*> ") ; Original
(setq geiser-repl-history-filename "~/.emacs.d/geiser-history")
