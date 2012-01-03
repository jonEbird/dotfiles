;; .emacs

;;; uncomment this line to disable loading of "fault.el" at startup
;; (setq inhibit-default-init t)

;; turn on font-lock mode
(when (fboundp 'global-font-lock-mode)
  (global-font-lock-mode t))

;; enable visual feedback on selections
;(setq transient-mark-mode t)

;; default to better frame titles
(setq frame-title-format
      (concat  "%b - emacs@" (system-name)))

;; default to unified diffs
(setq diff-switches "-u")

;; always end a file with a newline
;(setq require-final-newline 'query)

;(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 ;'(auto-compression-mode t nil (jka-compr))
 ;'(case-fold-search t)
 ;'(current-language-environment "UTF-8")
 ;'(default-input-method "rfc1345")
 ;'(global-font-lock-mode t nil (font-lock))
 ;'(remote-shell-program "ssh")
 ;'(show-paren-mode t nil (paren))
 ;'(transient-mark-mode t))
;(custom-set-faces
  ;;; custom-set-faces was added by Custom.
  ;;; If you edit it by hand, you could mess it up, so be careful.
  ;;; Your init file should contain only one such instance.
  ;;; If there is more than one, they won't work right.
 ;)

(autoload 'css-mode "~/css-mode")
(setq auto-mode-alist       
     (cons '("\\.css\\'" . css-mode) auto-mode-alist))

(autoload 'javascript-mode "~/javascript.el")
(setq auto-mode-alist       
     (cons '("\\.js\\'" . javascript-mode) auto-mode-alist))

;; not only turn off the bell but turn any of them off
(setq visible-bell 1)
(setq ring-bell-function 'ignore)

(global-set-key (kbd "<f2>") 'call-last-kbd-macro)
(global-set-key (kbd "S-<f2>")  'apply-macro-to-region-lines)

;; (autoload 'c-outline "~/c-outline" nil t)
;; (add-hook 'c-mode-hook 'c-outline)

;; tags customization
; (global-set-key (kbd "M-.") 'tags-search)

;; outline mode
(add-hook 'outline-mode-hook 'hide-body)

;; set/use X clipboard
(setq x-select-enable-clipboard t)

;; put scroll bar on the right
(set-scroll-bar-mode 'right)

;; include buffer name in titlebar
; (setq frame-title-format "%b - emacs")
(setq frame-title-format '(buffer-file-name "%f" ("%b")))

;; add a end newline
(setq require-final-newline 't)

(global-set-key "\C-cg" 'goto-line)
(global-set-key "\C-cG" 'goto-char)
(global-set-key "\C-cw" 'delete-region) ; ala C-w and M-C-w
(global-set-key "\C-cc" 'comment-region)
(global-set-key "\C-cu" 'uncomment-region)

;; don't iconify from within X
(when window-system
  (global-unset-key "\C-z")) ; iconify-or-deiconify-frame (C-x C-z)

;;;; Fun little startup animation
;; (defconst animate-n-steps 3)
;; (defun emacs-reloaded ()
;;   (animate-string (concat ";; Initialization successful, welcome to "
;;   			  (substring (emacs-version) 0 16)
;; 			  ".")
;; 		  0 0)
;;   (newline-and-indent)  (newline-and-indent))
;; (add-hook 'after-init-hook 'emacs-reloaded)  

(setq load-path (cons "/usr/share/gtags" load-path))
(autoload 'gtags-mode "gtags.el" "" t)
(setq c-mode-hook '(lambda () (gtags-mode 1) ))

; (gtags-visit-rootdir "/usr/src/linux-2.6.21")

(defun my-make-backup-file-name (file-name)
  "Create the non-numeric backup file name for `file-name'.\n
   This customized version of this function is useful to keep all backups
   in one place, instead of all over the filesystem."
  (require 'dired)
  (message (concat "make-backup: " file-name)) ; wtf?
  (if (file-exists-p "~/.backups")
      (concat (expand-file-name "~/.backups/")
	      (dired-replace-in-string "/" "|" file-name))
    (concat file-name "~")))

(setq make-backup-file-name-function 'my-make-backup-file-name)

;; Have revert-buffer not ask
(defun revert-buffer-noask()
  (interactive)
  (revert-buffer nil t))
(global-set-key (quote [f5]) (quote revert-buffer-noask))

(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; For longlines-mode
(setq longlines-wrap-follows-window-size t)

;; turn off the toolbar and menu bar
(tool-bar-mode -1)
(menu-bar-mode -1)


;(setq load-path (cons "/usr/share/emacs/site-lisp/slime" load-path))
;(setq inferior-lisp-program "sbcl")
;(require 'slime)
;(slime-setup)
;; (setq inferior-lisp-program "/usr/bin/sbcl")
