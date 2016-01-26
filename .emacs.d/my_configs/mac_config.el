;;; package --- Summary

;;; Commentary:

;;; Mac Configuration --- Summary
;;;   Setting up settings only for Macintosh

;; --------------------------------------------------
;; Now actually set the variables and more complicated setups
;; --------------------------------------------------

;;; Code:

(when (eq system-type 'darwin)
  (message "Setting up specific settings for Mac")

  ;; Consider the following setting if you end up not using
  ;; `exec-path-from-shell-copy-env'
  ;; (add-to-list 'exec-path "/usr/local/bin")

  (setq mac-command-modifier 'meta
        mac-option-modifier 'super
        mac-function-modifier 'control)

  ;; Have a big monitor but still like a vertical split
  (setq split-height-threshold nil
        split-width-threshold 160)

  ;; (setq org-file-apps '((auto-mode . emacs)
  ;;       		("\\.mm\\'" . default)
  ;;       		("\\.x?html?\\'" . default)
  ;;       		("\\.pdf\\'" . default)
  ;;       		("\\.xlsx?" . default)
  ;;       		("\\.docx?" . default)))

  ;; Need to properly set a Font
  ;; (set-face-attribute 'default nil
  ;;       	      :family "Consolas" :height 100)
  )

(provide 'mac_config)
;;; mac_config.el ends here
