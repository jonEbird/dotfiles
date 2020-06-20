;;; package --- Summary

;;; Commentary:

;; Configure development for Puppet

;;; Code:

(require 'puppet-mode)

(setq puppet-lint-command
      "puppet-lint --with-context --log-format \"%{path}:%{line}: %{kind}: %{message} (%{check})\"")

(defun my-puppet-mode-hook ()
  "Minor setup when entering Puppet mode."
  (interactive)
  (robe-mode)
  (hs-minor-mode)
  (highlight-thing-mode)
  (ac-flyspell-workaround)
  (flyspell-prog-mode)
  (rainbow-delimiters-mode))

(add-hook 'puppet-mode-hook 'my-puppet-mode-hook)

(provide 'puppet_config)
;;; puppet_config.el ends here
