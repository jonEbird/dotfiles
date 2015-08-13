;; .emacs
;; Jon Miller <jonEbird at gmail.com>

;; --------------------------------------------------
;; Basic variable configurations
;; --------------------------------------------------

;; Extra load path
(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp/"))

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
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)

; default to unified diffs
(setq diff-switches "-u")

; Helps my email package and/or other multi-lingual files
(set-language-environment "UTF-8")

; always end a file with a newline
(setq require-final-newline 't)

; Shorten the yes-or-no question
(defalias 'yes-or-no-p 'y-or-n-p)
(defalias 'repl 'ielm "Alias to the elisp REPL")

; Useful for conditional variables
(defvar hostname
  (or (getenv "HOSTNAME") (getenv "COMPUTERNAME") "unknown")
  "hostname of this machine")

; Session Save Support - Desktop and Savehist
(require 'desktop)
(require 'savehist)
(setq
 desktop-base-file-name (concat (expand-file-name "~/.emacs.d/desktop.") hostname)
 desktop-base-lock-name (concat (expand-file-name "~/.emacs.d/desktop.") hostname ".lock")
 savehist-file (concat (expand-file-name "~/.emacs.d/history.") hostname)
 history-length 250)
(desktop-save-mode 1)
(savehist-mode 1)

(add-to-list 'desktop-globals-to-save 'file-name-history)

; ELPA package support
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
  (add-to-list 'package-archives '("ELPA" . "http://tromey.com/elpa/"))
  (add-to-list 'package-archives '("MELPA" . "http://melpa.milkbox.net/packages/"))
  (package-initialize)
  )

;; Extra add-ons - Typcially from git submodules
; nyan-mode
;; (add-to-list 'load-path (expand-file-name "~/.emacs.d/nyan-mode/"))
;; (require 'nyan-mode)
;; (setq nyan-wavy-trail 't
;;       nyan-animate-nyancat 't
;;       nyan-bar-length 20)

;; Always show columns too
(column-number-mode)

;; Support an easier way to remember how to zoom in/out font size
;; Using M-mouse-wheel-up to increase and M-mouse-wheel-down to decrease
(global-set-key (kbd "<C-mouse-4>") (lambda () (interactive) (text-scale-increase 1)))
(global-set-key (kbd "<C-mouse-5>") (lambda () (interactive) (text-scale-decrease 1)))

;; Banish the mouse
(mouse-avoidance-mode 'banish)

;; Support a moderate scroll via pgdn (aka [next]) and pgup (aka [prior])
;; Useful for browsing log output or moderately shifting email for reading
(global-set-key [next]
                (lambda ()
                  (interactive)
                  (dolist (n '(6 2 1))
                    (scroll-up n)
                    (sit-for (/ 1.0 (+ n 20))))))
(global-set-key [prior]
                (lambda ()
                  (interactive)
                  (dolist (n '(6 2 1))
                    (scroll-down n)
                    (sit-for (/ 1.0 (+ n 20))))))

;; not only turn off the bell but turn any of them off
(setq visible-bell 1)
(setq ring-bell-function 'ignore)

;; set/use X clipboard
(setq x-select-enable-clipboard t)

;; I may enable flyspell-mode. When I do, let's kill the underline.
(eval-after-load "flyspell"
  '(progn
     (setq flyspell-issue-welcome-flag nil)
     ;; Older face name had "-face" extension
     (ignore-errors (set-face-underline-p 'flyspell-incorrect-face nil))
     (ignore-errors (set-face-underline-p 'flyspell-duplicate-face nil))
     ;; Newer 24.4 dropped the "-face"
     (ignore-errors (set-face-underline-p 'flyspell-incorrect nil))
     (ignore-errors (set-face-underline-p 'flyspell-duplicate nil))))
; Most of my flyspell hook are located in their own respective config files,
;   but for some modes I don't have a dedicated .el file
(add-hook 'text-mode-hook 'turn-on-flyspell 'append)

;; Make files that should be executable, executable
(defun jsm:make-buffer-file-executable-if-script-p ()
  "Limit the situations that I want scripts to be made executable"
  (interactive)
  (let ((parent-dir (file-name-base
                     (directory-file-name
                      (file-name-directory buffer-file-name)))))
    (if (cond ((eq major-mode 'sh-mode) t)
              ((and (eq major-mode 'python-mode)
                    (string= parent-dir "scripts")) t)
              (t nil))
        (executable-make-buffer-file-executable-if-script-p))))

(add-hook 'after-save-hook
          'jsm:make-buffer-file-executable-if-script-p)

;; In addition to flyspell use abbrev to help with my common spelling mistakes
; Define new abbreviations via: "C-x a i g" for global or "C-x a i l" for
; local modes and then don't forget to save your abbreviations via M-x
; write-abbrev-file. My other workflow is to:
; 1. Open abbrev-file via M-x list-abbrevs,
; 2. edit and save via abbrev-edit-save-buffer picking
;    ~/.emacs.d/abbrev_defs then you can close the buffer.
(dolist (hook '(erc-mode-hook
                emacs-lisp-mode-hook
                text-mode-hook
                org-mode-hook))
  (add-hook hook (lambda () (abbrev-mode 1))))
(quietly-read-abbrev-file)
(setq save-abbrevs 'silently)

;; Special, helper abbrevs
(define-abbrev global-abbrev-table "0mq" "ØMQ")

;; Damn my spelling / typing
(define-abbrev global-abbrev-table "teh" "the")

;; don't iconify from within X
(when (not (eq nil window-system))
  (global-unset-key "\C-z")) ; iconify-or-deiconify-frame (C-x C-z)

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
(setq delete-trailing-whitespace-modes (list "org-mode" "text-mode")) ; "emacs-lisp-mode"
(defun delete-trailing-whitespace-inmodes ()
  "Conditionally execute delete-trailing-whitespace if you are in a desired major-mode"
  (interactive)
  (if (member (symbol-name major-mode) delete-trailing-whitespace-modes)
      (delete-trailing-whitespace)
    ; For all else, at least show trailing whitespace
    (setq show-trailing-whitespace t)))
(add-hook 'before-save-hook 'delete-trailing-whitespace-inmodes)

; Bookmarks support
; See http://emacs-fu.blogspot.com/2009/11/bookmarks.html
(setq
 bookmark-default-file "~/.emacs.d/bookmarks" ;; keep my ~/ clean
 bookmark-save-flag 1)                        ;; autosave each change)

;; Games setup
(setq tetris-score-file (expand-file-name "~/.emacs.d/tetris-scores"))

;; Move customizations to a separate file
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

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
(add-hook 'conf-unix-mode-hook ; keep ace-jump in configs
          '(lambda () (local-set-key (kbd "C-c SPC") 'ace-jump-mode)))

; I like C-PGUP for previous-buffer and C-PGDN for next-buffer
(global-set-key [C-prior] 'previous-buffer)
(global-set-key [C-next] 'next-buffer)

;; I like to do a desktop-save in each of my projects
;(desktop-read (read-file-name "Which project desktop ? "))
; desktop-change-dir rip off?
; emacsclient -e "(desktop-read \"$(pwd)\")"

;; devilspie *.ds files are s-expressions
(add-to-list 'auto-mode-alist '("\\.ds$" . emacs-lisp-mode))

;; zsh files are to be treated as shell scripts
(add-to-list 'auto-mode-alist '("\\.zsh$" . sh-mode))

;; Markdown files
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))

;; Systemd unit files
(add-to-list 'auto-mode-alist '("\\.service" . conf-mode))

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

;; Dired setup
(eval-after-load 'dired
  '(define-key dired-mode-map (kbd "W") 'woman-dired-find-file))

;; Emacs 24.4 features
(add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode)
(setq load-prefer-newer t
      initial-buffer-choice 'remember-notes)

;; --------------------------------------------------
;; Extra stuff not significant enough to be in own file. Aka. Hacks
;; --------------------------------------------------

;; Xah Lee's colorization trick - http://ergoemacs.org/emacs/emacs_CSS_colors.html
(defun colorize-buffer ()
  "Syntax color hex color spec such as 「#ff1100」 in current buffer."
  (interactive)
  (font-lock-add-keywords
   nil
   '(("#[abcdef[:digit:]]\\{6\\}"
      (0 (put-text-property
          (match-beginning 0)
          (match-end 0)
          'face (list :background (match-string-no-properties 0)))))))
  (font-lock-fontify-buffer))

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

; Add unhighlight - hi-lock-unface-buffer ? hi-lock-interactive-patterns
(defun jsm:highlight-current-word(b e)
  (interactive "r")
  (highlight-phrase
   (format "\\<%s\\>" (buffer-substring-no-properties b e))
   'hi-yellow))
; I do not use mark-paragraph
(global-set-key (kbd "M-h") 'jsm:highlight-current-word)

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
(defalias 'ascii-unescape-buffer 'ansi-color-buffer)

;; Excorporate - Support for retrieving calendaring info from Exchange
;; Note: Need to load this before any other soap-clients like org-jira form my org_config
;; FIXME: This is an ugly place to put this
;(require 'excorporate)
;(setq excorporate-configuration "jsmiller@qti.qualcomm.com")

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
	  (with-demoted-errors "Error: %S"
            (load fullpath))
	(message "Could NOT load config file:%s" file))
      )))
; load my configuration files
(jsm:load-config-file '("el_get"
                        "lisp_config"
                        ;; "yasnippet_config"
                        "backup_config"
                        "org_config"
                        "tramp_config"
                        "erc_config"
                        "misc_languages"
                        "efficiency_config"
                        ;; Pick one: ido or helm
                        ; "ido_config"
                        "helm_config"
                        "email_config"
                        ; "elip_edb"
                        "c-c++_tags_config"
                        "python_config"
                        "php_config"
                        "linux_config"
                        "windows_config"
                        ;; "screencast"
                        ))

(setq frame-title-format '(buffer-file-name "%f" ("%b")))
