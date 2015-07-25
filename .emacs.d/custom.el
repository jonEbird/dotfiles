;; Leave this here for whenever I use M-x customize-variable.
;;  when related to a major config, I may move it manually.
;;  until I move it, this will be a dropping zone
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "31a01668c84d03862a970c471edbd377b2430868eccf5e8a9aec6831f1a0908d" "1297a022df4228b81bc0436230f211bad168a117282c20ddcba2db8c6a200743" "756597b162f1be60a12dbd52bab71d40d6a2845a3e3c2584c6573ee9c332a66e" "0fe229019b6395a78aefe7dd673d909b7aa89edb22bb6e077a94d9dcaee2de21" "6a37be365d1d95fad2f4d185e51928c789ef7a4ccf17e7ca13ad63a8bf5b922f" "3a727bdc09a7a141e58925258b6e873c65ccf393b2240c51553098ca93957723" "e4e97731f52a5237f37ceb2423cb327778c7d3af7dc831788473d4a76bcc9760" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" default)))
 '(display-time-mode t)
 '(fill-column 75)
 '(flymake-log-level 3)
 '(global-visual-line-mode t)
 '(gtags-prefix-key "C-t")
 '(helm-gtags-auto-update t)
 '(helm-gtags-ignore-case t)
 '(helm-gtags-path-style (quote relative))
 '(helm-gtags-prefix-key "C-c")
 '(helm-gtags-suggested-key-mapping t)
 '(org-drill-optimal-factor-matrix
   (quote
    ((2
      (2.36 . 2.412)
      (2.5 . 2.5)
      (1.96 . 2.238)
      (2.2800000000000002 . 2.407)
      (2.46 . 2.496)
      (2.6 . 2.588)
      (2.7 . 2.679))
     (1
      (2.1799999999999997 . 3.72)
      (1.7000000000000002 . 3.44)
      (2.5 . 4.0)
      (2.36 . 3.86)
      (2.6 . 4.14)))))
 '(safe-local-variable-values
   (quote
    ((whitespace-style face tabs trailing lines-tail)
     (require-final-newline)
     (rpm-change-log-uses-utc . t)
     (Encoding . utf-8)
     (eval ignore-errors "Write-contents-functions is a buffer-local alternative to before-save-hook"
           (add-hook
            (quote write-contents-functions)
            (lambda nil
              (delete-trailing-whitespace)
              nil))
           (require
            (quote whitespace))
           "Sometimes the mode needs to be toggled off and on."
           (whitespace-mode 0)
           (whitespace-mode 1))
     (whitespace-line-column . 80)
     (whitespace-style face trailing lines-tail)
     (require-final-newline . t)
     (encoding . utf-8))))
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(uniquify-buffer-name-style (quote post-forward) nil (uniquify))
 '(virtualenv-root (expand-file-name "~/venv/"))
 '(visual-line-mode 1 t))
;; Same story for this block. Just leave it here for now.
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ediff-current-diff-C ((t (:background "gold4"))))
 '(ediff-fine-diff-B ((t (:background "dark green"))))
 '(flyspell-incorrect ((t (:inherit error :foreground "firebrick2" :inverse-video nil :underline nil :slant normal :weight normal))))
 '(org-code ((t (:inherit shadow :foreground "forest green"))))
 '(org-level-5 ((t (:inherit outline-5 :foreground "dark orange"))))
 '(org-level-6 ((t (:inherit outline-6 :foreground "gold"))))
 '(org-verbatim ((t (:inherit shadow :foreground "dark green" :underline t))))
 '(sh-heredoc ((t (:foreground "#2aa198")))))
