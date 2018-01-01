;;; company_config --- Summary

;;; Commentary:

;;; Code:
(global-company-mode)

;; Language setup
(add-to-list 'company-backends 'company-jedi)

;; Misc Setup
(require 'readline-complete)
(push 'company-readline company-backends)
(add-hook 'rlc-no-readline-hook (lambda () (company-mode -1)))

(provide 'company-config)
;;; company_config.el ends here
