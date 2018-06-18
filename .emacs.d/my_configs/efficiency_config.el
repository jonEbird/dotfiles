; -*- emacs-lisp -*-

;;; Code:

; General custom config to help efficiency within the Editor. So, this
; could be many different things from project navigation, to custom
; function with key bindings, etc.
(require 'use-package)

;; Git Setup
;; ------------------------------

;; 1. Magit support

(use-package magit
  :bind ("C-x C-z" . magit-status)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1
                                 "Full screen magit mode."))

;; 2. Linking to git commits
(use-package orgit
  :custom
  (orgit-remote "upstream" "Default remote used when exporting links.")
  :config
  (add-to-list 'orgit-export-alist
               '("github.pie.apple.com[:/]\\(.+?\\)\\(?:\\.git\\)?$"
                 "https://github.pie.apple.com/%n"
                 "https://github.pie.apple.com/%n/commits/%r"
                 "https://github.pie.apple.com/%n/commit/%r")))

;; always follow symlinks to files under source-control. dont ask.
(setq vc-follow-symlinks t)

;; 3. Enable git-gutter
(use-package git-gutter
  :custom
  (git-gutter:disabled-modes '(org-mode mu4e-view-mode mu4e-headers-mode)
                             "Do not use git-gutter with org files or email.")
  :hook (c-mode-hook c++-mode-hook python-mode-hook emacs-lisp-mode-hook shell-mode-hook)
  :bind (("C-x C-g" . git-gutter-mode)
         ("C-x v =" . git-gutter:popup-hunk)
         ("C-x v p" . git-gutter:previous-hunk)
         ("C-x v n" . git-gutter:next-hunk)
         ("C-x v s" . git-gutter:stage-hunk)
         ("C-x v r" . git-gutter:revert-hunk)))

;; Avy - Replacing previously used ace-jump-mode
;; ------------------------------
(use-package avy
  :bind (("C-0" . avy-goto-word-1)
         ("C-:" . avy-goto-char)
         ("C-'" . avy-goto-char-2)
         ("M-g g" . avy-goto-line)))

;; Expand-region
;; ------------------------------
(use-package expand-region
  :bind ("C-=" . er/expand-region))

;; Enable transposing of windows to be much easier
;; Thanks to http://emacswiki.org/emacs/TransposeWindows
;; ------------------------------
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

;; Install yaml-mode via the ELPA repository and associate file types
(use-package yaml-mode)

;; Guide Key - Woot
;; ------------------------------
(use-package guide-key
  :custom (guide-key/guide-key-sequence
           '("C-x r" ; registers
             "C-x 4" ; window
             "C-x 5" ; frame
             "C-c p" "C-c p s" "C-c p 4" ; projectile
             "C-c h" ; help
             "C-x g" ; google-this
             "C-c @" ; hide-show
             ))
  :config (guide-key-mode 1))

;; Visual Regexp Replacements
;; ------------------------------
(use-package visual-regexp
  :bind ("C-M-%" . vr/query-replace))

;; Projectile Project Management
;; -----------------------------
(defun github-browse ()
  "Use the 'hub' utility to quickly open up the project webpage in GHE."
  (interactive)
  (let ((default-directory (projectile-project-root))
        (cmd (format "hub browse")))
    (shell-command cmd nil nil)))

;; Integrate with projectile
(defun projectile-utility-shell ()
  "Launch a utility shell."
  (interactive)
  (funcall (faux-screen-utility-terminal "prj") (projectile-project-root)))

(use-package projectile
  :custom ((projectile-enable-caching t)
           (projectile-indexing-method 'alien)
           (projectile-tags-command "ctags -Re -f \"%s\" %s")
           (projectile-tags-file-name "TAGS")
           (projectile-buffers-filter-function 'projectile-buffers-with-file)
           (projectile-mode-line '(:eval "")))
  :config (progn
            (projectile-global-mode)
            (add-to-list 'projectile-globally-ignored-modes "helm.*")
            (add-to-list 'projectile-globally-ignored-modes "magit.*")
            (add-to-list 'projectile-globally-ignored-directories "gems")
            (use-package ripgrep
              :config
              (def-projectile-commander-method ?A
                "Find rg on project."
                (call-interactively 'projectile-ripgrep))))
  :bind (("C-M-s" . jsm/projectile-counsel-rg)
         :map projectile-command-map
              ("B" . 'github-browse)
              ("$" . 'projectile-utility-shell)
              ("s s" . jsm/projectile-counsel-rg)))

;; Need to pass 't to compilation-start to enable comint minor mode for *compilation* buffer
(defun projectile-run-compilation (cmd)
  "Run external or Elisp compilation command CMD."
  (if (functionp cmd)
      (funcall cmd)
    (compilation-start cmd t)))

;; Ack support with ack-and-a-half
;; ------------------------------
(use-package ack-and-a-half)

;; README preview helper thanks to pandoc
;; ------------------------------
(defun readme-preview ()
  "Preview the README rendered to html in a browser tab via pandoc."
  (interactive)
  (let* ((html-filename (make-temp-file "preview"))
         (input-format (cond
                        ((derived-mode-p 'rst-mode) "rst")
                        ((derived-mode-p 'markdown-mode) "markdown_github")))
         (cmd (format "pandoc -f %s -t html -o %s %s"
                      input-format html-filename buffer-file-name))
         (browse-url-browser-function 'browse-url-generic)
         (browse-url-generic-program "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"))
    (shell-command cmd nil nil)
    (browse-url (concat "file://" html-filename))
    (run-at-time "10 sec" nil `(lambda () (delete-file ,html-filename)))))

;; Assign F12 to render README preview for select modes
(dolist (hook '(markdown-mode-hook rst-mode-hook))
  (add-hook hook
            '(lambda () (local-set-key (kbd "<f12>") 'readme-preview))))

;; Multiple-cursors
;; ------------------------------
(use-package multiple-cursors
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C--" . mc/mark-all-like-this)
         ("C-S-<mouse-1>" . mc/add-cursor-on-click)))

;; Allow isearch functionality with multiple-cursors
;; -------------------------------------------------
(use-package phi-search
  :custom (phi-search-limit 10000)
  :config (progn
            (add-hook 'multiple-cursors-mode-enabled-hook
                      (lambda ()
                        (interactive)
                        (global-set-key (kbd "C-s") 'phi-search)
                        (global-set-key (kbd "C-r") 'phi-search-backward)))
            (add-hook 'multiple-cursors-mode-disabled-hook
                      (lambda ()
                        (interactive)
                        (global-set-key (kbd "C-s") 'isearch-forward)
                        (global-set-key (kbd "C-r") 'isearch-backward)))))

;; Smart Searching
;; ------------------------------
;; Just discovered "M-s ." and only then learned of the vim equivalent
(global-set-key (kbd "C-*") 'isearch-forward-symbol-at-point)

;; I also like to use occur, so following in the same manor
(defun jsm/occur-thing-at-point ()
  "Start an occur with the `thing-at-point'."
  (interactive)
  (occur (format "\\<%s\\>" (thing-at-point 'symbol t))))

(global-set-key (kbd "M-s *") 'jsm/occur-thing-at-point)
(global-set-key (kbd "C-M-*") 'jsm/occur-thing-at-point)

;; More search replacements
;; Per http://pragmaticemacs.com/emacs/dont-search-swipe/
(use-package swiper
  :custom ((ivy-display-style 'fancy)
           (ivy-use-virtual-buffers t))
  :bind (("s-s" . swiper)
         ("s-r" . swiper)))

;; Undo-tree - Try for starters: C-x u
;; ------------------------------
(use-package undo-tree
  :config (global-undo-tree-mode))

;; I like using autopair for all modes
;; ------------------------------
;; (use-package autopair
;;   :custom (autopair-autowrap t)
;;   :config (autopair-global-mode 1))

;; TODO: Other idea for org-mode is to hack defun ispell-command-loop to
;;       add a new command option to "wrap(w)" or "codify(c)" the word and
;;       let it wrap it with a either "~" or "=" values.

;; Replaces the need for autopair
(use-package elec-pair
  :custom (electric-pair-pairs
           '(
             (?\" . ?\")
             (?\{ . ?\})
             (?\~ . ?\~)  ; This is for org-mode to wrap in verbatim
             ))
  :config (electric-pair-mode 1))

;; Moved onto using session over desktop.el
;; ------------------------------
;; (setq session-save-file (concat (expand-file-name "~/.emacs.d/.session-") hostname))
;; (require 'session)
;; (add-hook 'after-init-hook 'session-initialize)
;; (setq desktop-globals-to-save '(desktop-missing-file-warning)) ;; per session.el

;; Improved buffer listing - Use ibuffer
;; ------------------------------
; (global-set-key (kbd "C-x C-b") 'ibuffer) ;; grew tired of this
(global-set-key (kbd "C-x C-b")
                (lookup-key (current-global-map) (kbd "C-x b")))

;; gist support
;; ------------------------------
; Try setting gh-profile-alist for private, Enterprise github usage

;; cmake support - http://www.cmake.org/Wiki/CMake/Editors/Emacs
;; ------------------------------
(use-package cmake-mode
  :mode (("CMakeLists\\.txt\\'" . cmake-mode)
         ("\\.cmake\\'" . cmake-mode)))

;; User defined select all
;; ------------------------------
;; Just never became a fan of C-x h to select the whole buffer
(defun select-all ()
  "Select all text in the buffer."
  (interactive)
  (save-excursion
    (mark-whole-buffer)
    (kill-ring-save (point-min) (point-max))))

;; Advice functions
;; ------------------------------

; Or just set this
(setq scroll-preserve-screen-position t)

;; Use my faux-screen library for shell support
;; --------------------------------------------

;; Shell setup
(setq explicit-shell-file-name "bash"
      explicit-bash-args '("-c" "export EMACS=; stty echo; bash")
      comint-process-echoes t)

(add-to-list 'load-path (expand-file-name "~/repos/faux-screen/"))
(setq faux-screen-num-terminals 10
      faux-screen-keymap-prefix (kbd "C-\\")
      faux-screen-terminal-ps1 "(\\[\\e[1;36m\\]%s\\[\\e[0m\\]) \\W $ ")
(require 'faux-screen)
(faux-screen-global-mode)
(global-set-key [C-next]  'faux-screen-next-dwim)
(global-set-key [C-prior] 'faux-screen-prev-dwim)
(faux-screen-terminals)

; (faux-screen-new-terminal "jerry" t)
; (faux-screen-new-terminal 10 t)
; (setq jon-shell (faux-screen-utility-terminal "jon"))
; (funcall (faux-screen-utility-terminal "jon"))
; (global-set-key (kbd "<f6>") (faux-screen-utility-terminal "jon"))
; (funcall jon-shell "~/repos")

;; Setup a utility terminal to be used in ad-hoc situations using the
;; current default-directory location
(global-set-key (kbd "<f12>")
                (lambda ()
                  (interactive)
                  (funcall (faux-screen-utility-terminal "Utility") default-directory)))

;; Recentf Support
;; ------------------------------
;; enable recent files mode.
(use-package recentf
  :custom (recentf-max-saved-items 100)
  :config (progn
            (recentf-mode t)
            (add-to-list 'recentf-exclude "^/tmp/.*html$")
            (add-to-list 'recentf-exclude "^/tmp/org")
            (add-to-list 'recentf-exclude (expand-file-name "~/Maildir"))))

;; Enable semantic mode to better support imenu
(use-package semantic
  :custom (semantic-edits-verbose-flag nil)
  :config (semantic-mode 1))

;; Capture my work window configuration and be able to switch back to it easily
;; ------------------------------
;; TODO

;; Show me bad whitespace
;; ------------------------------
;; (require 'whitespace)
;; (setq whitespace-style '(face tabs trailing indentation space-before-tab space-after-tab)
;;       whitespace-global-modes '(python-mode c-mode c++-mode))
;; (global-whitespace-mode 1)
; (face tabs spaces trailing lines space-before-tab newline indentation empty space-after-tab space-mark tab-mark newline-mark)

;; Support for showing key commands
(use-package command-log-mode
  :custom ((command-log-mode-window-size 55)
           (command-log-mode-open-log-turns-on-mode t)))

;; Default key-binding, once command-log-mode is enabled, is "C-c o"
(defun log-keys ()
  "Enable command-log-mode and show keys."
  (interactive)
  (command-log-mode 1)
  (clm/command-log-clear)
  (clm/open-command-log-buffer))

(defun log-keys-disable ()
  "Disable command-log-mode."
  (interactive)
  (command-log-mode -1))

;; ediff setup
;; ------------------------------
;; Thanks http://oremacs.com/2015/01/17/setting-up-ediff/
(defmacro csetq (variable value)
  `(funcall (or (get ',variable 'custom-set)
                'set-default)
            ',variable ,value))
(csetq ediff-window-setup-function 'ediff-setup-windows-plain)
(csetq ediff-split-window-function 'split-window-horizontally)
(csetq ediff-diff-options "-w")
(csetq ediff-show-clashes-only t)

(defun my-ediff-cleanup ()
  (interactive)
  (ediff-janitor nil nil))

(add-hook 'ediff-cleanup-hook 'my-ediff-cleanup)

;; TODO: Move from winner-mode to leveraging `ediff-before-setup-hook' and
;; then restore it from `ediff-quit-hook' and `ediff-suspend-hook'

(use-package winner
  :config (winner-mode))

;; (add-hook 'ediff-after-quit-hook-internal 'winner-undo)

;; (setq my-window-config (current-window-configuration))
;; (set-window-configuration my-window-config)

; 1. Saving current working window config
; 2. Jumping back to working window config
; Would be nice to support a ring of window configurations
; F10         - Cycle through window configs
; C-u F10     - Add current window config to collection
; C-u C-u F10 - Remove current window configuration

;; (require 'winring)
;; (winring-initialize)

;; Quickly split window and get to my most common org files
;; ------------------------------
(defun switch-to-myfile-other-window (filename)
  "Generate a function for quickly switching to or opening FILENAME in a `other-window' split style."
  `(lambda ()
     (interactive)
     (let* ((filepath ,(expand-file-name filename))
            (filebuffer (file-name-nondirectory filepath)))
       (if (get-buffer filebuffer)
           (switch-to-buffer-other-window filebuffer)
         (find-file-other-window filepath)))))

;; Key chords - http://www.emacswiki.org/emacs/key-chord.el
;; ------------------------------
(use-package key-chord
  :config (progn
          (key-chord-mode 1)
          (key-chord-define-global ",."  "<>\C-b")
          (key-chord-define-global "JP"  (switch-to-myfile-other-window "~/org/projects.org"))
          (key-chord-define-global "JI"  (switch-to-myfile-other-window "~/org/info.org"))
          (key-chord-define-global "JM"  (switch-to-myfile-other-window "~/org/meetings.org"))
          (key-chord-define-global "JT"  (switch-to-myfile-other-window "~/org/tasks.org"))
          (key-chord-define-global "JS"  (switch-to-myfile-other-window "~/org/secret.gpg"))
          (key-chord-define-global "HL"  (lambda () (interactive) (hs-hide-level 0)))
          (key-chord-define-global "HS"  (lambda () (interactive) (hs-toggle-hiding)))))

;; IRC chat over a mosh connection to my VPS
;; Using `mtrace' for buffer notification
(use-package mtrace
  :custom (mtrace-notify-changes-limit 1)
  :config (mtrace-mode))

(defun mosh-irc ()
  (interactive)
  (if (get-buffer "*Shell mosh-irc*")
      (if (equal (buffer-name) "*Shell mosh-irc*")
          (bury-buffer)
        (switch-to-buffer "*Shell mosh-irc*"))
    (funcall (faux-screen-utility-terminal "mosh-irc") (expand-file-name "~"))))
(key-chord-define-global "CT" (lambda () (interactive) (mosh-irc)))

(use-package define-word)

;; Dired Fixup
;; ------------------------------
;; http://mbork.pl/2015-04-25_Some_Dired_goodies
(setq dired-guess-shell-alist-user
      '(("\\.pdf\\'" "evince")
	("\\.tex\\'" "pdflatex")
	("\\.ods\\'\\|\\.xlsx?\\'\\|\\.docx?\\'\\|\\.csv\\'" "libreoffice")))

;; Don't keep dired buffers around
(put 'dired-find-alternate-file 'disabled nil)

(add-to-list 'completion-ignored-extensions ".snapshot/")
(use-package dired-x)
(use-package wdired)

;;narrow dired to match filter
;; (use-package dired-narrow
;;   :ensure t
;;   :bind (:map dired-mode-map
;;               ("/" . dired-narrow)))

;; Reusing buffers is about using `find-alternative-file'
(add-hook 'dired-mode-hook
          (lambda ()
            (define-key dired-mode-map (kbd "^") ; was dired-up-directory
              (lambda () (interactive) (find-alternate-file "..")))))

;; Zeal http://zealdocs.org/
;; ------------------------------
(use-package zeal-at-point
  :bind ("s-z" . zeal-at-point)
  :config (progn
            (add-to-list 'zeal-at-point-mode-alist '(python-mode . "python 2"))
            (add-to-list 'zeal-at-point-mode-alist '(emacs-lisp-mode . "emacs lisp"))))

;; Open Files within containers
;; ------------------------------
;; http://www.emacswiki.org/emacs/TrampAndDocker

(use-package docker-tramp
  :init (push
         (cons
          "docker"
          '((tramp-login-program "docker")
            (tramp-login-args (("exec" "-it") ("%h") ("/bin/bash")))
            (tramp-remote-shell "/bin/sh")
            (tramp-remote-shell-args ("-i") ("-c"))))
         tramp-methods))

(defadvice tramp-completion-handle-file-name-all-completions
  (around dotemacs-completion-docker activate)
  "(tramp-completion-handle-file-name-all-completions \"\" \"/docker:\" returns
    a list of active Docker container names, followed by colons."
  (if (equal (ad-get-arg 1) "/docker:")
      (let* ((dockernames-raw (shell-command-to-string "docker ps | awk '$NF != \"NAMES\" { print $NF \":\" }'"))
             (dockernames (cl-remove-if-not
                           #'(lambda (dockerline) (string-match ":$" dockerline))
                           (split-string dockernames-raw "\n"))))
        (setq ad-return-value dockernames))
    ad-do-it))

;; Ace-window
;; ------------------------------
(use-package ace-window
  :custom (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  :bind ("C-x o" . ace-window))

;; Ensure Emoji characters are correctly displayed
(defun --set-emoji-font (frame)
  "Adjust the font settings of FRAME so Emacs can display emoji properly."
  (if (eq system-type 'darwin)
      ;; For NS/Cocoa
      (set-fontset-font t 'symbol (font-spec :family "Apple Color Emoji") frame 'prepend)
    ;; For Linux
    (set-fontset-font t 'symbol (font-spec :family "Symbola") frame 'prepend)))

;; For when Emacs is started in GUI mode:
(--set-emoji-font nil)
;; Hook for when a frame is created with emacsclient
;; see https://www.gnu.org/software/emacs/manual/html_node/elisp/Creating-Frames.html
(add-hook 'after-make-frame-functions '--set-emoji-font)

;; Evil mode for my Boss For people that want to use my keyboard, to help
;; point out something that I'm editing but are unfortunately vim users,
;; use the following F13 key (DAS keyboard) to toggle between evil-mode.
;; (use-package evil
;;   :bind ("<f13>" . evil-mode))

;; Instead lets use F13 for toggling tree view for navigation

(defun neotree-project-dir ()
  "Open NeoTree using the git root."
  (interactive)
  (let ((project-dir (projectile-project-root))
        (file-name (buffer-file-name)))
    (neotree-toggle)
    (if project-dir
        (if (neo-global--window-exists-p)
            (progn
              (neotree-dir project-dir)
              (neotree-find file-name)))
      (message "Could not find git project root."))))

(use-package emacs-neotree
  :bind ("<f13>" . neotree-project-dir))

;; Quickly launch Google searches
(use-package google-this
  :bind ("C-x g" . google-this-mode-submap)
  :custom (google-this-mode 1))

;; Highlight TODO, FIXME, etc
(use-package hl-todo
  :config (global-hl-todo-mode))

;; Use the Helpful package
;; https://github.com/Wilfred/helpful
(use-package helpful
  :bind (("C-h f" . #'helpful-callable)
         ("C-h v" . #'helpful-variable)
         ("C-h k" . #'helpful-key)
         ("C-c C-." .  #'helpful-at-point)))

(provide 'efficiency_config)
;;; efficiency_config.el ends here
