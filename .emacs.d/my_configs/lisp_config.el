; -*- emacs-lisp -*-

(require 'rainbow-delimiters)

(defun jsm/lisp-modes-hook ()
  "Hook this single function to the lisp-like modes"
  (interactive)
  (paredit-mode)
  (rainbow-delimiters-mode)
  (git-gutter-mode)
  (eldoc-mode 1)
  (define-key paredit-mode-map (kbd "M-q") 'fill-paragraph)
  (flyspell-prog-mode))

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

;; Help out elisp hacking
(define-key emacs-lisp-mode-map (kbd "C-c .") 'find-function)

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
          (setq cider-repl-result-prefix ";; => "
                cider-repl-history-file "~/.emacs.d/cider-repl.history"
                org-babel-clojure-backend 'cider)))
; cider-known-endpoints
