; -*- emacs-lisp -*-

;;(load-file "~/projects/stackoverflow/flymake.el")
;; (when (load "flymake" t)
;;   (defun flymake-pyflakes-init ()
;;     (let* ((temp-file (flymake-init-create-temp-buffer-copy
;; 		       'flymake-create-temp-inplace))
;; 	   (local-file (file-relative-name
;; 			temp-file
;; 			(file-name-directory buffer-file-name))))
;;       (list "pyflakes" (list local-file))))
;;   (add-to-list 'flymake-allowed-file-name-masks
;; 	       '("\\.py\\'" flymake-pyflakes-init)))
;;(add-hook 'find-file-hook 'flymake-find-file-hook)

; Jedi auto completion support
(if (require 'jedi nil 'noerror)
  ; (add-hook 'python-mode-hook 'auto-complete-mode 'append)
  (add-hook 'python-mode-hook 'jedi:setup)
  (setq jedi:setup-keys t
        jedi:complete-on-dot t))
