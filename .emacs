;; .emacs
;; Jon Miller <jonEbird at gmail.com>

;; --------------------------------------------------
;; Basic variable configurations
;; --------------------------------------------------

;; Extra load path
; (add-to-list 'load-path (expand-file-name "~/.emacs.d/"))

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
 desktop-base-file-name (concat (expand-file-name "~/.emacs.desktop.") hostname)
 desktop-base-lock-name (concat (expand-file-name "~/.emacs.desktop.") hostname ".lock")
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
     (if (boundp 'flyspell-incorrect-face) (set-face-underline-p 'flyspell-incorrect-face nil))
     (if (boundp 'flyspell-duplicate-face) (set-face-underline-p 'flyspell-duplicate-face nil))))
; Most of my flyspell hook are located in their own respective config files,
;   but for some modes I don't have a dedicated .el file
(add-hook 'text-mode-hook 'turn-on-flyspell 'append)

;; In addition to flyspell use abbrev to help with my common spelling mistakes
; Define new abbreviations via: "C-x a i g" for global or "C-x a i l" for
; local modes and then don't forget to save your abbreviations via M-x
; write-abbrev-file. My other workflow is to:
; 1. Open abbrev-file via M-x list-abbrevs,
; 2. edit and save via abbrev-edit-save-buffer picking
;    ~/.emacs.d/abbrev_defs then you can close the buffer.
;; (dolist (hook '(erc-mode-hook
;;                 emacs-lisp-mode-hook
;;                 text-mode-hook
;;                 org-mode-hook
;;                 mu4e-compose-mode-hook))
;;   (add-hook hook (lambda () (abbrev-mode 1))))
;; (quietly-read-abbrev-file)
; (setq save-abbrevs 'silently)

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

;; Leave this here for whenever I use M-x customize-variable.
;;  when related to a major config, I may move it manually.
;;  until I move it, this will be a dropping zone
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("756597b162f1be60a12dbd52bab71d40d6a2845a3e3c2584c6573ee9c332a66e" "0fe229019b6395a78aefe7dd673d909b7aa89edb22bb6e077a94d9dcaee2de21" "6a37be365d1d95fad2f4d185e51928c789ef7a4ccf17e7ca13ad63a8bf5b922f" "3a727bdc09a7a141e58925258b6e873c65ccf393b2240c51553098ca93957723" "e4e97731f52a5237f37ceb2423cb327778c7d3af7dc831788473d4a76bcc9760" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" default)))
 '(display-time-mode t)
 '(fill-column 75)
 '(flymake-log-level 3)
 '(global-visual-line-mode t)
 '(org-drill-optimal-factor-matrix
   (quote
    ((2
      (2.36 . 2.412)
      (2.5 . 2.5)
      (1.96 . 2.238)
      (2.2800000000000002 . 2.407)
      (2.46 . 2.496)
      (2.6 . 2.588)
      (2.7 . 2.679))
     (1
      (2.1799999999999997 . 3.72)
      (1.7000000000000002 . 3.44)
      (2.5 . 4.0)
      (2.36 . 3.86)
      (2.6 . 4.14)))))
 '(safe-local-variable-values
   (quote
    ((require-final-newline)
     (rpm-change-log-uses-utc . t)
     (Encoding . utf-8)
     (eval ignore-errors "Write-contents-functions is a buffer-local alternative to before-save-hook"
           (add-hook
            (quote write-contents-functions)
            (lambda nil
              (delete-trailing-whitespace)
              nil))
           (require
            (quote whitespace))
           "Sometimes the mode needs to be toggled off and on."
           (whitespace-mode 0)
           (whitespace-mode 1))
     (whitespace-line-column . 80)
     (whitespace-style face trailing lines-tail)
     (require-final-newline . t)
     (encoding . utf-8))))
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(uniquify-buffer-name-style (quote post-forward) nil (uniquify))
 '(virtualenv-root (expand-file-name "~/venv/"))
 '(visual-line-mode 1 t))
;; Same story for this block. Just leave it here for now.
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flyspell-incorrect ((t (:inherit error :foreground "firebrick3"))))
 '(org-level-5 ((t (:inherit outline-5 :foreground "dark orange"))))
 '(org-level-6 ((t (:inherit outline-6 :foreground "gold"))))
 '(sh-heredoc ((t (:foreground "#2aa198")))))

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
(jsm:load-config-file '("el_get"
                        "lisp_config"
                        ; "yasnippet_config"
                        "backup_config"
                        "php_config"
                        "org_config"
                        "c-c++_tags_config"
                        "python_config"
                        "tramp_config"
                        "windows_config"
                        "erc_config"
                        "misc_languages"
                        "efficiency_config"
                        ; "ido_config"
                        "helm_config"
                        "email_config"
                        ; "elip_edb"
                        "linux_config"
                        ))

(setq frame-title-format '(buffer-file-name "%f" ("%b")))
