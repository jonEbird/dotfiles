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
  ; (smartparens-mode t)
  (google-set-c-style)
  (google-make-newline-indent)
  (flycheck-pmd-setup)
  (meghanada-mode t)
  (company-mode t)
  (make-variable-buffer-local 'compilation-scroll-output)
  (setq tab-width 4
        c-basic-offset 4
        compilation-scroll-output t))

(use-package flycheck-pmd
  :custom (flycheck-pmd-rulesets
           '("java-basic" "java-design" "java-imports" "java-braces" "java-codesize" "java-unusedcode"))
  :hook (java-mode . flycheck-pmd-setup))

(use-package mustache-mode
  :mode ("\\.mustache" . mustache-mode))

;; (use-package autodisass-java-bytecode
;;   :ensure t
;;   :defer t)

(use-package google-c-style
  :defer t
  :ensure t
  :commands
  (google-set-c-style))

;; Kotlin support
(defun my-kotlin-mode-hook ()
  "Minor setup when entering Kotlin mode."
  (interactive)
  (hs-minor-mode)
  (flyspell-prog-mode)
  (flycheck-mode 1)
  (highlight-thing-mode)
  (rainbow-delimiters-mode)
  (setq tab-width 4))

(use-package kotlin-mode
  :hook (kotlin-mode . my-kotlin-mode-hook))

(use-package meghanada
  :defer t
  :init
  (add-hook 'java-mode-hook 'my-java-mode-hook)

  :config
  (use-package realgud
    :ensure t)
  :custom ((meghanada-server-remote-debug t)
           (meghanada-javac-xlint "-Xlint:all,-processing"))
  :bind
  (:map meghanada-mode-map
        ("C-S-t" . meghanada-switch-testcase)
        ("M-RET" . meghanada-local-variable)
        ("M-r" . meghanada-reference)
        ("M-t" . meghanada-typeinfo)
        ("C-z" . hydra-meghanada/body))
  :commands
  (meghanada-mode))

(defhydra hydra-meghanada (:hint nil :exit t)
"
^Edit^                           ^Tast or Task^
^^^^^^-------------------------------------------------------
_f_: meghanada-compile-file      _m_: meghanada-restart
_c_: meghanada-compile-project   _t_: meghanada-run-task
_o_: meghanada-optimize-import   _j_: meghanada-run-junit-test-case
_s_: meghanada-switch-test-case  _J_: meghanada-run-junit-class
_v_: meghanada-local-variable    _R_: meghanada-run-junit-recent
_i_: meghanada-import-all        _r_: meghanada-reference
_g_: magit-status                _T_: meghanada-typeinfo
_l_: counsel-git
_q_: exit
"
  ("f" meghanada-compile-file)
  ("m" meghanada-restart)

  ("c" meghanada-compile-project)
  ("o" meghanada-optimize-import)
  ("s" meghanada-switch-test-case)
  ("v" meghanada-local-variable)
  ("i" meghanada-import-all)

  ("g" magit-status)
  ("l" counsel-git)

  ("t" meghanada-run-task)
  ("T" meghanada-typeinfo)
  ("j" meghanada-run-junit-test-case)
  ("J" meghanada-run-junit-class)
  ("R" meghanada-run-junit-recent)
  ("r" meghanada-reference)

  ("q" exit)
  ("z" nil "leave"))

(provide 'java_config)
;;; java_config.el ends here
