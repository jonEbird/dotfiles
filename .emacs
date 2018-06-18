;; .emacs
;; Jon Miller <jonEbird at gmail.com>

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
; (package-initialize)

;; --------------------------------------------------
;; Basic variable configurations
;; --------------------------------------------------
;; Extra load path
(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp/"))

;; Require cl early on since it's used in many places
(require 'cl)

; turn off the toolbar and menu bar
(tool-bar-mode -1)
(menu-bar-mode -1)

;; https://emacs.stackexchange.com/questions/28736/emacs-pointcursor-movement-lag/28746
(setq auto-window-vscroll nil)

; tabs are evil
(setq-default indent-tabs-mode nil)

; turn on font-lock mode
(when (fboundp 'global-font-lock-mode)
  (global-font-lock-mode t))

; enable visual feedback on selections
(setq transient-mark-mode t)

; include buffer name in titlebar
(setq frame-title-format '(buffer-file-name "%f" ("%b"))
      inhibit-default-init t ;; Needed to avoid Fedora's default.el overriding my frame-title-format setting
      )
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)

; default to unified diffs
(setq diff-switches "-u")

; Helps my email package and/or other multi-lingual files
(set-language-environment "UTF-8")

; always end a file with a newline
(setq require-final-newline 't)

;; compilation windows should scroll to first error
(setq compilation-scroll-output 'first-error)

; Shorten the yes-or-no question
(defalias 'yes-or-no-p 'y-or-n-p)
(defalias 'repl 'ielm "Alias to the elisp REPL")
(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  "Prevent annoying \"Active processes exist\" query when you quit Emacs."
  (flet ((process-list ())) ad-do-it))

; Useful for conditional variables
(defvar my-hostname
  (or
   (getenv "HOSTNAME")
   (getenv "COMPUTERNAME")
   (shell-command-to-string "printf %s \"$(uname -n)\"")
   "unknown")
  "Hostname of this machine.")

;; Session Save Support - Desktop and Savehist
;; -------------------------------------------
;; (require 'desktop)
;; (add-to-list 'desktop-globals-to-save 'file-name-history)
(require 'savehist)
(setq history-length 250
      desktop-base-file-name (concat (expand-file-name "~/.emacs.d/desktop.") my-hostname)
      desktop-base-lock-name (concat (expand-file-name "~/.emacs.d/desktop.") my-hostname ".lock")
      savehist-file (concat (expand-file-name "~/.emacs.d/history.") my-hostname))
;; (desktop-save-mode 1)
(savehist-mode 1)

; ELPA package support
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
  (add-to-list 'package-archives '("ELPA" . "http://tromey.com/elpa/"))
  (add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
  ; (add-to-list 'package-archives '("MELPA" . "http://melpa.milkbox.net/packages/"))
  )

(when (>= emacs-major-version 25)
  (setq search-default-mode 'case-fold-search)
  )

;; Extra add-ons - Typcially from git submodules
; nyan-mode
;; (add-to-list 'load-path (expand-file-name "~/.emacs.d/nyan-mode/"))
;; (require 'nyan-mode)
;; (setq nyan-wavy-trail 't
;;       nyan-animate-nyancat 't
;;       nyan-bar-length 20)

;; Always show columns too and set the default fill width to 90
(setq fill-column 90)
(column-number-mode)

;; Support an easier way to remember how to zoom in/out font size
;; Using M-mouse-wheel-up to increase and M-mouse-wheel-down to decrease
(global-set-key (kbd "<C-mouse-4>") (lambda () (interactive) (text-scale-increase 1)))
(global-set-key (kbd "<C-mouse-5>") (lambda () (interactive) (text-scale-decrease 1)))

;; Quickly kill a buffer
(global-set-key (kbd "M-k") (lambda () (interactive) (kill-buffer nil)))
;; Or just bury it?
(global-set-key [C-M-backspace] (lambda () (interactive) (bury-buffer nil)))

;; Banish the mouse
(mouse-avoidance-mode 'none)

;; Support a moderate scroll via pgdn (aka [next]) and pgup (aka [prior])
;; Useful for browsing log output or moderately shifting email for reading
(defun jsm/smooth-scroll (direction)
  (interactive)
  ;; Down is up and up is down in this crazy mixed up world
  (let ((f (if (equal direction 'up) 'scroll-down 'scroll-up)))
    (dolist (n '(6 2 1))
      (funcall f n)
      (sit-for (/ 1.0 (+ n 20))))))

(global-set-key [next] (lambda () (interactive) (jsm/smooth-scroll 'down)))
(global-set-key [prior] (lambda () (interactive) (jsm/smooth-scroll 'up)))
(when (eq system-type 'darwin)
  ;; On a Mac, my smooth scrolling when pgdn is C-down and pgup is C-up
  (global-set-key [C-down] (lambda () (interactive) (jsm/smooth-scroll 'down)))
  (global-set-key [C-up] (lambda () (interactive) (jsm/smooth-scroll 'up))))

;; not only turn off the bell but turn any of them off
(setq visible-bell t)
(setq ring-bell-function 'ignore)

;; set/use X clipboard
(setq x-select-enable-clipboard t)

;; Transparency
(defun toggle-transparency ()
  (interactive)
  (let* ((alpha-frame-parameter (frame-parameter nil 'alpha))
         (alpha-transparency
          (if (eq alpha-frame-parameter nil)
              100
            (cadr alpha-frame-parameter))))
    (if (/= alpha-transparency 100)
        (set-frame-parameter nil 'alpha '(100 100))
      (set-frame-parameter nil 'alpha '(90 50)))))
(toggle-transparency)
(global-set-key (kbd "C-c t") 'toggle-transparency)

;; Replace old linenum with native support from v26 onward
;; (setq display-line-numbers t)
;; Actually it's just: (display-line-numbers-mode)

;; Setup my preferred frame geometry upon startup
;; TODO: Consider using (display-monitor-attributes-list) or (frame-monitor-attributes)
(defun jsm/set-frame-geometry ()
  "Re-size and move frame to my pre-set values for monitor or laptop viewing."
  (interactive)
  (let* ((large (> (x-display-pixel-width) 2000))
         (frame-height (if large 80 50))  ;; was 75 50
         (frame-width (if large 280 178)) ;; was 260 178
         (x-offset 0)
         (y-offset 0))
    (set-frame-height (selected-frame) frame-height)
    (set-frame-width (selected-frame) frame-width)
    (set-frame-position (selected-frame) x-offset y-offset)))

(jsm/set-frame-geometry)

;; I may enable flyspell-mode. When I do, let's kill the underline.
(defvar flyspell-issue-welcome-flag nil)
(eval-after-load "flyspell"
  '(progn
     ;; Older face name had "-face" extension
     (ignore-errors (set-face-underline 'flyspell-incorrect-face nil))
     (ignore-errors (set-face-underline 'flyspell-duplicate-face nil))
     ;; Newer 24.4 dropped the "-face"
     (ignore-errors (set-face-underline 'flyspell-incorrect nil))
     (ignore-errors (set-face-underline 'flyspell-duplicate nil))))
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
    (if (cond ((and (eq major-mode 'sh-mode)
                    (string= (file-name-extension buffer-file-name) ".sh")) t)
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
                org-mode-hook
                prog-mode-hook))
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
(defvar longlines-wrap-follows-window-size t)

;; Show the datetime in the modeline?
; display-time-format overrides display-time-day-and-date & display-time-24hr-format
(defvar display-time-format "%a %Y-%m-%d %H:%M")
(defvar display-time-load-average-threshold 5)
; Enabling the modeline addition (Disable with a negative ARG)
(display-time-mode 1)

;; Start server for emacsclient
(server-start nil)

;; Kill trailing whitespace but only in certain modes
(defvar delete-trailing-whitespace-modes '("org-mode" "text-mode"))
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
(defvar bookmark-default-file "~/.emacs.d/bookmarks"
  "keep my ~/ clean")
(defvar bookmark-save-flag 1 "autosave each change")

;; Games setup
(defvar tetris-score-file (expand-file-name "~/.emacs.d/tetris-scores"))

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
(add-to-list 'auto-mode-alist '("\\.bats$" . sh-mode))

;; Markdown files
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))

;; Systemd unit files
(add-to-list 'auto-mode-alist '("\\.service" . conf-mode))

;; GnuPG Setup
(defvar epa-armor 't)
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
(global-set-key (kbd "s-h") 'jsm:highlight-current-word)

;; And speaking of highlighting results
(setq grep-highlight-matches t)

;; (unhighlight-regexp t)

(defun jsm:kill-ring-sexp(&optional arg)
  "Highlight the current sexp ala mark-sexp and then copy ala kill-ring-save"
  (interactive "^p")
  (or arg (setq arg 1))
  (mark-sexp arg)
  (kill-ring-save (region-beginning) (region-end)))
;(global-set-key "\M-p" 'jsm:kill-ring-sexp)

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
(defconst jsm:emacs-config-dir "~/.emacs.d/my_configs/"
  "Directory path to my configs.")

;; Utility function to auto-load my package configurations
(defun jsm:load-config-file (config)
  "Load separate CONFIG file."
  (let ((fullpath (expand-file-name
                   (concat jsm:emacs-config-dir config ".el"))))
    (if (file-exists-p fullpath)
        (with-demoted-errors "Error: %S"
          (load fullpath))
      (message "Could NOT load config file:%s" config))))

(defun jsm:profile-config-file (config)
  "Profile one of your separate CONFIG files."
  (interactive "s")
  (let ((profile-dotemacs-file
         (expand-file-name (concat jsm:emacs-config-dir config ".el"))))
    (profile-dotemacs)))

;; Move customizations to a separate file
;; Running this after any of the other configs so our custom items aren't actually overridden
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; (jsm:profile-config-file "el_get")
;; (jsm:profile-config-file "org_config")
;; (jsm:profile-config-file "efficiency_config")

;; Leaving these as separate function calls provides for better profiling
(jsm:load-config-file "el_get")
(jsm:load-config-file "lisp_config")
;; (jsm:load-config-file "yasnippet_config")
(jsm:load-config-file "backup_config")
(jsm:load-config-file "org_config")
(jsm:load-config-file "tramp_config")
(jsm:load-config-file "erc_config")
;; Pick one: ido or helm or ivy
;; (jsm:load-config-file "ido_config")
;; (jsm:load-config-file "helm_config")
(jsm:load-config-file "ivy_config")
(jsm:load-config-file "misc_languages")
(jsm:load-config-file "efficiency_config")
(jsm:load-config-file "theme_config")
(jsm:load-config-file "email_config")
;;(jsm:load-config-file "elip_edb")
(jsm:load-config-file "java_config")
(jsm:load-config-file "scala_config")
(jsm:load-config-file "c-c++_tags_config")
(jsm:load-config-file "python_config")
(jsm:load-config-file "php_config")
(jsm:load-config-file "ruby_config")
(jsm:load-config-file "shell_config")
(jsm:load-config-file "linux_config")
(jsm:load-config-file "windows_config")
(jsm:load-config-file "mac_config")
(jsm:load-config-file "company_config")
;; (jsm:load-config-file "screencast")

(provide '.emacs)
;;; .emacs ends here
