; -*- emacs-lisp -*-

;; Inspiration from Doom
;; https://github.com/hlissner/doom-emacs/tree/develop/modules/lang/javascript

;; https://github.com/ananthakumaran/tide

(defun my-tide-mode-hook ()
  (interactive)
  (tide-setup)
  (flycheck-mode 1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled)
        company-tooltip-align-annotations t)
  (flycheck-add-next-checker 'javascript-eslint 'javascript-tide 'append)
  (eldoc-mode 1)
  (tide-hl-identifier-mode +1)
  (company-mode 1))

(defun my-js-mode-hook ()
  (interactive)
  (hs-minor-mode)
  (flyspell-prog-mode)
  (flycheck-mode 1)
  (git-gutter-mode 1)
  (highlight-thing-mode))

;; Javascript
(use-package js2-mode
  :mode "\\.js\\'"
  :custom ((js-indent-level 2)
           (js2-strict-missing-semi-warning nil)))

(use-package typescript-mode
  :hook #'my-tide-mode-hook
  :custom ((typescript-indent-level 2)))

(use-package css-mode
  :custom ((css-indent-offset 2)))

(defun my-web-mode-hook ()
  (let ((ext (file-name-extension buffer-file-name))
        (included-ext '("tsx" "jsx")))
    (if (member ext included-ext)
        (my-tide-mode-hook))

    ;; enable typescript-tslint checker
    (flycheck-add-mode 'typescript-tslint 'web-mode)))

(use-package web-mode
  :mode (("\\.tsx\\'" . web-mode)
         ("\\.jsx\\'" . web-mode))
  :hook #'my-web-mode-hook)

(use-package js2-mode
  :hook #'my-tide-mode-hook)

(use-package tide
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save)))
