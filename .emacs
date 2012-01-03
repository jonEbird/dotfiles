;; .emacs
;; Jon Miller <jonEbird at gmail.com>

;; --------------------------------------------------
;; Basic variable configurations
;; --------------------------------------------------

; turn off the toolbar and menu bar
(tool-bar-mode -1)
(menu-bar-mode -1)

; turn on font-lock mode
(when (fboundp 'global-font-lock-mode)
  (global-font-lock-mode t))

; enable visual feedback on selections
(setq transient-mark-mode t)
(setq show-paren-mode t)

; include buffer name in titlebar
(setq frame-title-format '(buffer-file-name "%f" ("%b")))

; default to unified diffs
(setq diff-switches "-u")

; always end a file with a newline
(setq require-final-newline 't)

; ELPA package support
(require 'package)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))

;; not only turn off the bell but turn any of them off
(setq visible-bell 1)
(setq ring-bell-function 'ignore)

;; set/use X clipboard
(setq x-select-enable-clipboard t)

;; don't iconify from within X
(when window-system
  (global-unset-key "\C-z")) ; iconify-or-deiconify-frame (C-x C-z)

;; Customized Variables
(setq inhibit-startup-message t)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'scroll-left 'disabled nil)

;; For longlines-mode
(setq longlines-wrap-follows-window-size t)

;; Start server for emacsclient
(server-start nil)

;; Kill trailing whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)

; Bookmarks support
; See http://emacs-fu.blogspot.com/2009/11/bookmarks.html
(setq
 bookmark-default-file "~/.emacs.d/bookmarks" ;; keep my ~/ clean
 bookmark-save-flag 1)                        ;; autosave each change)

; Extend the history length
(setq history-length 250)

; Desktop support
(desktop-load-default)
(setq desktop-enable t)
(desktop-save-mode 1)
(add-to-list 'desktop-globals-to-save 'file-name-history)

;; Leave this here for whenever I use M-x customize-variable.
;;  when related to a major config, I may move it manually.
;;  until I move it, this will be a dropping zone
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(fill-column 100)
 '(flymake-log-level 3)
)

;; --------------------------------------------------
;; Key (re)assignments
;; --------------------------------------------------

(global-set-key (kbd "<f2>") 'call-last-kbd-macro)
(global-set-key (kbd "S-<f2>")  'apply-macro-to-region-lines)

(global-set-key "\C-cg" 'goto-line)
(global-set-key "\C-cG" 'goto-char)
(global-set-key "\C-cw" 'delete-region) ; ala C-w and M-C-w
(global-set-key "\C-cc" 'comment-region)
(global-set-key "\C-cu" 'uncomment-region)

;; I like to do a desktop-save in each of my projects
;(desktop-read (read-file-name "Which project desktop ? "))
; desktop-change-dir rip off?
; emacsclient -e "(desktop-read \"$(pwd)\")"

;; --------------------------------------------------
;; Extra stuff not significant enough to be in own file. Aka. Hacks
;; --------------------------------------------------

;; Have revert-buffer not ask assigned to F5
(defun revert-buffer-noask()
  (interactive)
  (revert-buffer nil t))
(global-set-key (quote [f5]) (quote revert-buffer-noask))

;; Taken from http://stackoverflow.com/questions/679275/sending-email-in-emacs-programs
(defun my-message-mail-region (b e to subject)
  "Send the current region in an email"
  (interactive "r\nsRecipient: \nsSubject: ")
  (let ((orig-buffer (current-buffer)))
    (message-mail to subject)
    (message-goto-body)
    (insert (save-excursion (set-buffer orig-buffer)
                            (buffer-substring-no-properties b e)))
    (message-send-and-exit)))

;; --------------------------------------------------
;; Load my personalized, modular extra elisp files
;; --------------------------------------------------

;; Taken from InitSplit (http://www.emacswiki.org/emacs/InitSplit)
;;  then modified slightly to test for the existence of config files
; default emacs configuration directory
(defconst jsm:emacs-config-dir "~/.emacs.d/my_configs/" "")
; utility finction to auto-load my package configurations
(defun jsm:load-config-file (filelist)
  (dolist (file filelist)
    (let ((fullpath (expand-file-name
		     (concat jsm:emacs-config-dir file))))
      (if (file-exists-p fullpath)
	  (load fullpath)
	(message "Could NOT load config file:%s" file))
      )))
; load my configuration files
(jsm:load-config-file '("php_config"
			"org_config"
			"c_config"
                        "backup_config"
			"python_config"
			"tramp_config"
			"windows_config"
                       ))
