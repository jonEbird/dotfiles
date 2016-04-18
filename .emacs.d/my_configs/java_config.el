;;; package --- Summary

;;; Commentary:

;; Configure development for Java

;;; Code:
(setq flycheck-pmd-rulesets
      '("java-basic" "java-design" "java-imports" "java-braces" "java-codesize" "java-unusedcode"))
(require 'flycheck-pmd)
(flycheck-pmd-setup)

(defun my-java-mode-hook ()
  "Minor setup when entering Java mode."
  (interactive)
  (hs-minor-mode)
  (flyspell-prog-mode)
  (flycheck-mode 1))

(add-hook 'java-mode-hook 'my-java-mode-hook)

;; (use-package java
;;              :config )

(require 'mustache-mode)
(add-to-list 'auto-mode-alist '("\\.mustache" . mustache-mode))

(provide 'java_config)
;;; java_config.el ends here
