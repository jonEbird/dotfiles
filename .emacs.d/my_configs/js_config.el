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
  (tide-hl-identifier-mode 1)
  (highlight-thing-mode 1)
  (company-mode 1))

(defun my-js-mode-hook ()
  (interactive)
  (tide-setup)
  (hs-minor-mode)
  (flyspell-prog-mode)
  (flycheck-mode 1)
  (git-gutter-mode 1)
  (highlight-thing-mode 1))

;; Javascript
(if (version< emacs-version "27.0")
    (use-package js2-mode
      :mode "\\.js\\'"
      :hook (js2-mode . my-js-mode-hook)
      :custom ((js-indent-level 2)
               (js2-strict-missing-semi-warning nil)))
  (use-package js
    :hook (js-mode . my-js-mode-hook)
    :custom ((js-indent-level 2))))


(use-package typescript-mode
  :hook (typescript-mode . my-tide-mode-hook)
  :custom ((typescript-indent-level 2)))

(use-package css-mode
  :custom ((css-indent-offset 2)))

(defun my-web-mode-hook ()
  (let ((ext (file-name-extension buffer-file-name))
        (included-ext '("tsx" "jsx")))
    (if (member ext included-ext)
        (my-tide-mode-hook))

    (highlight-thing-mode 1)
    ;; enable typescript-tslint checker
    (flycheck-add-mode 'typescript-tslint 'web-mode)))

(use-package web-mode
  :mode (("\\.tsx\\'" . web-mode)
         ("\\.jsx\\'" . web-mode))
  :custom ((web-mode-code-indent-offset 2))
  :hook (web-mode . my-web-mode-hook))

(use-package tide
  :after (typescript-mode company flycheck)
  :hook ((before-save . tide-format-before-save)))
