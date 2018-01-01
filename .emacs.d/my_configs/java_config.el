;;; package --- Summary

;;; Commentary:

;; Configure development for Java

;;; Code:
(defun my-java-mode-hook ()
  "Minor setup when entering Java mode."
  (interactive)
  (hs-minor-mode)
  (flyspell-prog-mode)
  (flycheck-mode 1)
  (highlight-thing-mode)
  (rainbow-delimiters-mode)
  (flycheck-pmd-setup))

(add-hook 'java-mode-hook 'my-java-mode-hook)

(use-package flycheck-pmd
  :custom (flycheck-pmd-rulesets
           '("java-basic" "java-design" "java-imports" "java-braces" "java-codesize" "java-unusedcode"))
  :hook (java-mode . flycheck-pmd-setup))

(use-package mustache-mode
  :mode ("\\.mustache" . mustache-mode))

(provide 'java_config)
;;; java_config.el ends here
