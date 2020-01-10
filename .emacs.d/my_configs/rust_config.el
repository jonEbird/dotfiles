; -*- emacs-lisp -*-

(use-package toml-mode
  :defer true)

(use-package flycheck-rust)

(use-package cargo
  :defer true)

;; For this to work, you need the Rust Language Server (RLS) installed
;; https://github.com/rust-lang/rls
;; 1. install rustup  : curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
;; 2. install nightly : rustup install nightly
;; 2. install rls     : rustup component add rls rust-analysis rust-src
;;    Or rust-analyzer instead
;; 3. install rustfmt: rustup component add rustfmt

(use-package lsp-mode
  :commands lsp
  ;; :requires yasnippet
  :config (progn
            (require 'lsp-clients)
            (require 'yasnippet))
  :defer true)

(use-package lsp-ui)

;; (use-package rust-mode
;;   :init
;;   (add-hook 'rust-mode-hook 'my-rust-mode-hook))

;; rustic-format-trigger 'on-save

;; In regards to auto-mode-alist, rust-mode is still being loaded first instead of rustic

;; Looking to rustic to complete the setup
;; https://github.com/brotzeit/rustic
(use-package rustic
  :custom ((rustic-format-trigger 'on-save))
  :init
  (add-hook 'rustic-mode-hook 'my-rust-mode-hook)
  :after (rust-mode))

;; (use-package racer)

(defun my-rust-mode-hook ()
  "Minor setup when entering Rust mode."
  (interactive)
  (hs-minor-mode)
  (flyspell-prog-mode)
  ;; (flycheck-mode 1)
  ;; (lsp)
  ;; (add-hook 'before-save-hook 'rustic-cargo-fmt)
  (cargo-minor-mode)
  (git-gutter-mode 1)
  (highlight-thing-mode)
  (flycheck-rust-setup)
  (ac-flyspell-workaround))

(provide 'rust_config)
;;; rust_config.el ends here
