;;; company_config --- Summary

;;; Commentary:

;;; Code:
(use-package company
  :custom ((company-idle-delay 0.3)
           (company-minimum-prefix-length 1))
  :config
  (global-company-mode)
  (setq company-backends (delete 'company-dabbrev company-backends)))

;; Misc Setup
(use-package readline-complete
  :custom ((company-minimum-prefix-length 1)
           (company-tooltip-align-annotations t))
  :config
  (push 'company-readline company-backends)
  (add-hook 'rlc-no-readline-hook (lambda () (company-mode -1))))

(use-package lsp-mode
  :hook (yaml-mode . lsp)
  :commands lsp)

(use-package lsp-yaml
  :config
  (puthash "file:///Users/jonmiller/repos/dchao/kw-to-json-schema/resources/rio.schema.json" "rio*.yml" lsp-yaml-schemas))

(use-package lsp-ui :commands lsp-ui-mode)
(use-package company-lsp :commands company-lsp)
(use-package dap-mode)
(use-package lsp-ivy)

;; (local-set-key (kbd "TAB") 'company-indent-or-complete-common)

(provide 'company-config)
;;; company_config.el ends here
