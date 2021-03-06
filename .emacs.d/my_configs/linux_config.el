; -*- emacs-lisp -*-

;;; Linux Configuration

;;; Code:

; Modified from: http://blog.binchen.org/posts/what-s-the-best-spell-check-set-up-in-emacs.html
(defun flyspell-detect-ispell-args (&optional RUN-TOGETHER)
  "if RUN-TOGETHER is true, spell check the CamelCase words"
  (let (args
        (ispell-program (file-name-nondirectory ispell-program-name)))
    (cond
     ((string= ispell-program "aspell")
      ;; force the English dictionary, support Camel Case spelling check (tested with aspell 0.6)
      (setq args (list "--sug-mode=ultra" "--lang=en_US"))
      (if RUN-TOGETHER
          (setq args (append args '("--run-together" "--run-together-limit=2" "--run-together-min=2")))))
     ((string= ispell-program "hunspell")
      (setq args nil)))
    args
    ))

(when (eq system-type 'gnu/linux)
  (message "Setting up specific settings for Linux")
  (setq browse-url-generic-program "google-chrome"
	browse-url-browser-function 'browse-url-generic)

  ; Setup spelling
  (cond
   ((executable-find "aspell")
    (setq ispell-program-name "aspell"
          ispell-extra-args (flyspell-detect-ispell-args)))
   ((executable-find "hunspell")
    (setq ispell-program-name "hunspell")
    (setq ispell-local-dictionary "en_US")
    (setq ispell-local-dictionary-alist
          '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil nil nil utf-8))))
   (t (setq ispell-program-name nil)))

  ; FIXME
  (setq org-file-apps '((auto-mode . emacs)
		      ("\\.mm\\'" . default)
		      ("\\.x?html?\\'" . "/opt/google/chrome/google-chrome %s")
		      ("\\.pdf\\'" . "/usr/bin/evince %s")) )
  ;; (load-theme 'tango-dark nil nil)
  ;; (eval-after-load "magit"
  ;;   '(set-face-attribute 'magit-item-highlight nil :foreground "#ffffff" :background "#3f4747"))
  ;; Consult Solarized color values at http://ethanschoonover.com/solarized#the-values
  (ignore-errors
    (load-theme my-dark-theme nil nil))
  (when (boundp 'helm-mode)
    (set-face-attribute 'helm-selection nil :background "dark green" :underline t)
    (set-face-attribute 'helm-source-header nil :background "#073642")
    (set-face-attribute 'helm-ff-directory nil :foreground "#d70000" :background "#073642"))

  ;; Printing support!
  (require 'printing)
  (pr-update-menus t)
  ; make sure we use localhost as cups server
  (setenv "CUPS_SERVER" "localhost")
  (require 'cups))

(provide 'linux_config)
;;; linux_config.el ends here
