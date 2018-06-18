;; Scala config

;;; Code:
(defun my-scala-mode-hook ()
  "Minor setup when entering Scala mode."
  (interactive)
  (show-paren-mode)
  (yas-minor-mode)
  (git-gutter-mode)
  (company-mode)
  (ensime-mode)
  (scala-mode:goto-start-of-code)
  (hs-minor-mode)
  (flyspell-prog-mode)
  (highlight-thing-mode)
  (rainbow-delimiters-mode))

(add-hook 'scala-mode-hook 'my-scala-mode-hook)

(use-package ensime
  :ensure t
  :pin melpa-stable
  :custom ((ensime-eldoc-hints 'all)
           (ensime-search-interface 'ivy)
           (ensime-startup-notification nil)))
;; (add-hook 'git-timemachine-mode-hook (lambda () (ensime-mode 0)))
;; Add (scala-mode:goto-start-of-code) to scala-mode-hook

(provide 'scala_config)
;;; scala_config.el ends here
