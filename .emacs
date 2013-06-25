;; .emacs
;; Jon Miller <jonEbird at gmail.com>

;; --------------------------------------------------
;; Basic variable configurations
;; --------------------------------------------------

;; Extra load path
(add-to-list 'load-path (expand-file-name "~/.emacs.d/"))

; turn off the toolbar and menu bar
(tool-bar-mode -1)
(menu-bar-mode -1)

; tabs are evil
(setq-default indent-tabs-mode nil)

; turn on font-lock mode
(when (fboundp 'global-font-lock-mode)
  (global-font-lock-mode t))

; enable visual feedback on selections
(setq transient-mark-mode t)

; include buffer name in titlebar
(setq frame-title-format '(buffer-file-name "%f" ("%b")))

; default to unified diffs
(setq diff-switches "-u")

; always end a file with a newline
(setq require-final-newline 't)

; Shorten the yes-or-no question
(defalias 'yes-or-no-p 'y-or-n-p)

; Useful for conditional variables
(defvar hostname
  (or (getenv "HOSTNAME") (getenv "COMPUTERNAME") "unknown")
  "hostname of this machine")

; ELPA package support
(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
  (add-to-list 'package-archives '("ELPA" . "http://tromey.com/elpa/"))
  (add-to-list 'package-archives '("MELPA" . "http://melpa.milkbox.net/"))
  )

;; Extra add-ons - Typcially from git submodules
; nyan-mode
(add-to-list 'load-path (expand-file-name "~/.emacs.d/nyan-mode/"))
(require 'nyan-mode)
(setq nyan-wavy-trail 't
      nyan-animate-nyancat 't
      nyan-bar-length 20)

;; not only turn off the bell but turn any of them off
(setq visible-bell 1)
(setq ring-bell-function 'ignore)

;; set/use X clipboard
(setq x-select-enable-clipboard t)

;; I may enable flyspell-mode. When I do, let's kill the underline.
(eval-after-load "flyspell"
  '(progn
     (set-face-underline-p 'flyspell-incorrect-face nil)
     (set-face-underline-p 'flyspell-duplicate-face nil)))
; Most of my flyspell hook are located in their own respective config files,
;   but for some modes I don't have a dedicated .el file
(add-hook 'text-mode-hook 'turn-on-flyspell 'append)

;; Printing support!
(require 'printing)
(pr-update-menus t)
; make sure we use localhost as cups server
(setenv "CUPS_SERVER" "localhost")
(require 'cups)

;; don't iconify from within X
(when (not (eq nil window-system))
  (global-unset-key "\C-z") ; iconify-or-deiconify-frame (C-x C-z)
  (nyan-mode))

;; Customized Variables
(setq inhibit-startup-message t)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'scroll-left 'disabled nil)

;; For longlines-mode
(setq longlines-wrap-follows-window-size t)

;; Show the datetime in the modeline?
; display-time-format overrides display-time-day-and-date & display-time-24hr-format
(setq display-time-format "%a %Y-%m-%d %H:%M"
      display-time-load-average-threshold 5)
; Enabling the modeline addition (Disable with a negative ARG)
(display-time-mode 1)

;; Start server for emacsclient
(server-start nil)

;; Kill trailing whitespace but only in certain modes
(setq delete-trailing-whitespace-modes (list "org-mode" "text-mode"))
(defun delete-trailing-whitespace-inmodes ()
  "Conditionally execute delete-trailing-whitespace if you are in a desired major-mode"
  (interactive)  
  (if (member (symbol-name major-mode) delete-trailing-whitespace-modes)
      (delete-trailing-whitespace)))
(add-hook 'before-save-hook 'delete-trailing-whitespace-inmodes)


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
(setq
 desktop-base-file-name (concat (expand-file-name "~/.emacs.desktop.") hostname)
 desktop-base-lock-name (concat (expand-file-name "~/.emacs.desktop.") hostname ".lock")
 )

(add-to-list 'desktop-globals-to-save 'file-name-history)

;; Enabling ido Mode
; Used http://www.masteringemacs.org/articles/2010/10/10/introduction-to-ido-mode/ for guidelines.
(setq ido-enable-flex-matching t
      ido-everywhere t
      ido-use-filename-at-point 'guess
      ido-create-new-buffer 'always
      ido-save-directory-list-file (concat (expand-file-name "~/.ido.last.") hostname)
      ido-file-extensions-order '(".org" ".txt" ".py" ".emacs" ".xml" ".el" ".ini" ".cfg" ".cnf"))
(ido-mode 1)

;; Enable transposing of windows to be much easier
;; Thanks to http://emacswiki.org/emacs/TransposeWindows
(defun transpose-windows (arg)
  "Transpose the buffers shown in two windows."
  (interactive "p")
  (let ((selector (if (>= arg 0) 'next-window 'previous-window)))
    (while (/= arg 0)
      (let ((this-win (window-buffer))
            (next-win (window-buffer (funcall selector))))
        (set-window-buffer (selected-window) next-win)
        (set-window-buffer (funcall selector) this-win)
        (select-window (funcall selector)))
      (setq arg (if (plusp arg) (1- arg) (1+ arg))))))
(define-key ctl-x-4-map (kbd "t") 'transpose-windows)

;; Using ace-jump
(require 'ace-jump-mode)
(global-set-key (kbd "C-c SPC") 'ace-jump-mode)

;; Leave this here for whenever I use M-x customize-variable.
;;  when related to a major config, I may move it manually.
;;  until I move it, this will be a dropping zone
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" default)))
 '(display-time-mode t)
 '(fill-column 75)
 '(flymake-log-level 3)
 '(global-visual-line-mode t)
 '(safe-local-variable-values (quote ((encoding . utf-8))))
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(virtualenv-root (expand-file-name "~/venv/"))
 '(visual-line-mode 1 t))
;; Same story for this block. Just leave it here for now.
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
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
(global-set-key (kbd "<f8>") 'flyspell-check-previous-highlighted-word)
(global-set-key "\M-1" 'string-insert-rectangle)

; I like C-PGUP for previous-buffer and C-PGDN for next-buffer
(global-set-key [C-prior] 'previous-buffer)
(global-set-key [C-next] 'next-buffer)

;; I like to do a desktop-save in each of my projects
;(desktop-read (read-file-name "Which project desktop ? "))
; desktop-change-dir rip off?
; emacsclient -e "(desktop-read \"$(pwd)\")"

;; devilspie *.ds files are s-expressions
(add-to-list 'auto-mode-alist '("\\.ds$" . emacs-lisp-mode))

;; Install yaml-mode via the ELPA repository and associate file types
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))

;; Markdown files
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))

;; Transparency
(eval-when-compile (require 'cl))
(set-frame-parameter (selected-frame) 'alpha '(100 100))
(add-to-list 'default-frame-alist '(alpha 100 100))
(defun toggle-transparency ()
  (interactive)
  (if (/=
       (cadr (find 'alpha (frame-parameters nil) :key #'car))
       100)
      (set-frame-parameter nil 'alpha '(100 100)) ; Not transparent
    (set-frame-parameter nil 'alpha '(80 60))))   ; Tranparent
(global-set-key (kbd "C-c t") 'toggle-transparency)

;; GnuPG Setup
(setq epa-armor 't)
(require 'epa-file)
(epa-file-enable)

;; --------------------------------------------------
;; Extra stuff not significant enough to be in own file. Aka. Hacks
;; --------------------------------------------------

;; Have revert-buffer not ask assigned to F5
(defun revert-buffer-noask()
  (interactive)
  (revert-buffer nil t))
(global-set-key (quote [f5]) (quote revert-buffer-noask))
; Stupid hack, but I've found when resizing windows re-enabling visual-line-mode helps
(global-set-key (kbd "<f6>") '(lambda () (interactive) (visual-line-mode 1)))

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

(defun jsm:highlight-current-word(b e)
  (interactive "r")
  (highlight-phrase
   (buffer-substring-no-properties b e)
   'hi-yellow))
; I do not use mark-paragraph
(global-set-key "\M-h" 'jsm:highlight-current-word)

(defun jsm:kill-ring-sexp(&optional arg)
  "Highlight the current sexp ala mark-sexp and then copy ala kill-ring-save"
  (interactive "^p")
  (or arg (setq arg 1))
  (mark-sexp arg)
  (kill-ring-save (region-beginning) (region-end)))
(global-set-key "\M-p" 'jsm:kill-ring-sexp)

;; Used for preferring a .gpg version of a file over a normal one
;;   Handy for dynamically building configs, such as org-mode during boot.
(defun jsm:prefer-gpg (filelist)
  (interactive)
  (if (listp filelist)
      (let (mylist (reverse filelist))
	(dolist (file filelist mylist)
	  (if (file-exists-p (expand-file-name (concat file ".gpg")))
	      (setq mylist (cons (expand-file-name (concat file ".gpg")) mylist))
	    (setq mylist (cons (expand-file-name file) mylist))))
	)
    ; filelist is not really a list
    (if (file-exists-p (expand-file-name (concat filelist ".gpg")))
	(expand-file-name (concat filelist ".gpg"))
      (expand-file-name filelist)
      )))

;; Convert the Region/buffer to a colorized face via existing ascii escapes
(defun ansi-color-buffer ()
  (interactive)
  (save-excursion
    (ansi-color-apply-on-region (point-min) (point-max))))

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
		     (concat jsm:emacs-config-dir file ".el"))))
      (if (file-exists-p fullpath)
	  (load fullpath)
	(message "Could NOT load config file:%s" file))
      )))
; load my configuration files
(jsm:load-config-file '("php_config"
			"org_config"
			"c_config"
                        "c++_config"
                        "backup_config"
			"python_config"
			"tramp_config"
			"linux_config"
			"windows_config"
			"erc_config"
                        "el_get"
                       ))

(setq frame-title-format '(buffer-file-name "%f" ("%b")))
