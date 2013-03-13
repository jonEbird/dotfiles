; -*- emacs-lisp -*-

;; I have gtags installed via ELPA package system
; (setq c-mode-hook '(lambda () (gtags-mode 1) ))

; (gtags-visit-rootdir "/usr/src/linux-2.6.21")

;; etags-select extensions
(require 'etags-select)
(require 'etags-table)
(global-set-key "\M-?" 'etags-select-find-tag-at-point)
(global-set-key "\M-." 'etags-select-find-tag)
; This helps you automatically search and find the TAGS file
(setq etags-table-search-up-depth 10)
; Also nice to be able to explictly remember the interactive function to visit another TAGS file
(defalias 'tags-visit-table 'visit-tags-table)

;; Taken from http://www.emacswiki.org/emacs/EtagsSelect
(defun my-ido-find-tag ()
  "Find a tag using ido"
  (interactive)
  (tags-completion-table)
  (let (tag-names)
    (mapatoms (lambda (x)
                (push (prin1-to-string x t) tag-names))
              tags-completion-table)
    (etags-select-find (ido-completing-read "Tag: " tag-names))))
;; Commented out for now. Nice but too slow for huge TAGS files such as for the Linux kernel
; (global-set-key (kbd "M-.") 'my-ido-find-tag)

; Set my coding style for working with other Linux source
(add-hook 'c-mode-hook
          '(lambda ()
             (c-set-style "linux")))

