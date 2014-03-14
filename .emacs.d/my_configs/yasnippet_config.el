; -*- emacs-lisp -*-

; NOTEs:
; 1. Per yas-snippet-dirs, our local snippets can be saved in ~/.emacs.d/snippets
; 2. Documentation is here: http://capitaomorte.github.io/yasnippet/

(require 'yasnippet)
(yas-global-mode 1)

; TODO - Read all of this: http://cx4a.org/software/auto-complete/manual.html
; Activate autocomplete here
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-config-default)

(ac-set-trigger-key "TAB")
(ac-set-trigger-key "<tab>")
