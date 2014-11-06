; -*- emacs-lisp -*-

; General custom config to help efficiency within the Editor. So, this
; could be many different things from project navigation, to custom
; function with key bindings, etc.

;; Git Setup
;; ------------------------------

;; 1. Magit support
(global-set-key (kbd "C-x C-z") 'magit-status)

;; 2. Enable the git-gutter
(setq git-gutter:disabled-modes
      '(org-mode mu4e-view-mode mu4e-headers-mode))

; (global-git-gutter-mode t)
(dolist (hook
         '(python-mode-hook c++-mode-hook c-mode-hook emacs-lisp-mode-hook shell-mode-hook))
  (add-hook hook 'git-gutter-mode 'append))

(global-set-key (kbd "C-x C-g") 'git-gutter:toggle)
(global-set-key (kbd "C-x v =") 'git-gutter:popup-hunk)

;; Jump to next/previous hunk
(global-set-key (kbd "C-x v p") 'git-gutter:previous-hunk)
(global-set-key (kbd "C-x v n") 'git-gutter:next-hunk)

;; Stage current hunk
(global-set-key (kbd "C-x v s") 'git-gutter:stage-hunk)

;; Revert current hunk
(global-set-key (kbd "C-x v r") 'git-gutter:revert-hunk)

;; Using ace-jump
;; ------------------------------
(require 'ace-jump-mode)
(global-set-key (kbd "C-c SPC") 'ace-jump-mode)
(global-set-key (kbd "C-0") 'ace-jump-mode)

;; Enabling ido Mode
;; ------------------------------
; Used http://www.masteringemacs.org/articles/2010/10/10/introduction-to-ido-mode/ for guidelines.
(setq ido-enable-flex-matching t
      ido-everywhere t
      ido-use-filename-at-point 'guess
      ido-create-new-buffer 'always
      ido-save-directory-list-file (concat (expand-file-name "~/.ido.last.") hostname)
      ido-file-extensions-order '(".org" ".txt" ".py" ".emacs" ".xml" ".el" ".ini" ".cfg" ".cnf")
      ido-ignore-buffers '("\\` " "^*mu4e-"))
(add-to-list 'ido-ignore-files "\\.emacs\\.desktop")
(ido-mode 1)

;; I can never remember which key to split vertically and horizontally as
;; well as thinking of it the opposite way
(global-set-key (kbd "C-x |") (lambda () (interactive) (split-window-right) (ido-switch-buffer-other-window)))
(global-set-key (kbd "C-x _") (lambda () (interactive) (split-window-below) (ido-switch-buffer-other-window)))

;; ido-imenu - More navigation help
;; ------------------------------
(require 'idomenu)
(global-set-key (kbd "C-x C-i") 'idomenu)

;; Improved IDO match support
;; ------------------------------
(require 'flx-ido)
(flx-ido-mode 1)
;; Disable default ido faces to see flx highlights
(setq ido-use-faces nil)
; Finally, with the improved highlighting, it is also nice to view matches vertically
(ido-vertical-mode)

;; Improved buffer listing - Use ibuffer
;; ------------------------------
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Expand-region
;; ------------------------------
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

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

;; Setup Multple Cursors
;; ------------------------------
; TODO

;; Guide Key - Woot
;; ------------------------------
(require 'guide-key)
(guide-key-mode 1)
(setq guide-key/guide-key-sequence '("C-x r" "C-x 4" "C-x 5" "C-c p"))

;; Visual Regexp Replacements
;; ------------------------------
(require 'visual-regexp)
; (define-key global-map (kbd "C-c r") 'vr/replace)
(define-key global-map (kbd "C-M-%") 'vr/query-replace)

;; Projectile Project Management
;; ------------------------------
(require 'projectile)
(projectile-global-mode)
(setq projectile-enable-caching t
      projectile-indexing-method 'native)

; I will periodically do the following to keep my dot-files project from
; cluttering projectile's caching:
; sed -n '/^[^#]/s/.*/-&/p' ~/.gitignore > ~/.projectile

; (setq projectile-tags-command "gtags %s") ; Was "ctags -Re %s"

; Launch a unique shell for the particular session or project
(defun jsm/unique-shell (&optional directory)
  "Start or return to a shell session named and started from a particular directory"
  (interactive)
  (let* ((basedir (or directory (read-directory-name "Base Directory: ")))
         (default-directory basedir))
    (shell (concat "*ProjSH* "
                   (file-name-base (replace-regexp-in-string "/*$" "" basedir))))))

(global-set-key (kbd "C-x 4 $") 'jsm/unique-shell)

;; Ack support with ack-and-a-half
;; ------------------------------
(require 'ack-and-a-half)
(defalias 'ack 'ack-and-a-half)
(defalias 'ack-same 'ack-and-a-half-same)
(defalias 'ack-find-file 'ack-and-a-half-find-file)
(defalias 'ack-find-file-same 'ack-and-a-half-find-file-same)

;; Markdown preview helper
;; ------------------------------
(require 'markdown-mode)
(defun jsm/markdown-preview ()
  "Preview the markdown file in a new browser tab"
  (interactive)
  (let ((my-filename (buffer-file-name))
        (html-filename (format "%s.html" (file-name-base (buffer-file-name)))))
    (shell-command (format "pandoc -f markdown_github -t html -o %s %s" html-filename my-filename) nil nil)
    (browse-url (concat "file://" (file-name-directory (buffer-file-name)) html-filename))))
(define-key markdown-mode-map (kbd "<f12>") 'jsm/markdown-preview)

;; Multiple-cursors
;; ------------------------------
(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)            ; works on active region
(global-set-key (kbd "C->") 'mc/mark-next-like-this)           ; "like-this" works when region on keyword
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C--") 'mc/mark-all-like-this)            ; recall you can scroll via M-v / C-v to view all cursors
(global-set-key (kbd "C-S-<mouse-1>") 'mc/add-cursor-on-click)

;; Allow isearch functionality with multiple-cursors
(require 'phi-search)
(setq phi-search-limit 10000)
(add-hook 'multiple-cursors-mode-enabled-hook
          (lambda ()
                 (interactive)
                 (global-set-key (kbd "C-s") 'phi-search)
                 (global-set-key (kbd "C-r") 'phi-search-backward)))
(add-hook 'multiple-cursors-mode-disabled-hook
          (lambda ()
                 (interactive)
                 (global-set-key (kbd "C-s") 'isearch-forward)
                 (global-set-key (kbd "C-r") 'isearch-backward)))
; (require 'phi-replace)
; (global-set-key (kbd "M-%") 'phi-replace-query)

;; Undo-tree - Try for starters: C-x u
;; ------------------------------
(global-undo-tree-mode)

;; I like using autopair for all modes
;; ------------------------------
(require 'autopair)
(autopair-global-mode 1)
(setq autopair-autowrap t)

;; Moved onto using session over desktop.el
;; ------------------------------
(setq session-save-file (concat (expand-file-name "~/.emacs.d/.session-") hostname))
(require 'session)
(add-hook 'after-init-hook 'session-initialize)
(setq desktop-globals-to-save '(desktop-missing-file-warning)) ;; per session.el

;; Smart-mode-line
;; ------------------------------
(require 'smart-mode-line)
(setq sml/theme 'respectful
      sml/shorten-modes t)
(add-to-list 'sml/replacer-regexp-list '("^/repos/" ":Repo:"))
(setq sml/hidden-modes '(" hl-p" " Undo-Tree" " Guide" " pair" " ARev"))

(sml/setup)

;; gist support
;; ------------------------------
; Try setting gh-profile-alist for private, Enterprise github usage


;; cmake support - http://www.cmake.org/Wiki/CMake/Editors/Emacs
;; ------------------------------
(require 'cmake-mode)
(setq auto-mode-alist
      (append
       '(("CMakeLists\\.txt\\'" . cmake-mode))
       '(("\\.cmake\\'" . cmake-mode))
       auto-mode-alist))

;; User defined select all
;; ------------------------------
;; Just never became a fan of C-x h to select the whole buffer
(defun select-all ()
  "Select all text in the buffer"
  (interactive)
  (save-excursion
    (mark-whole-buffer)
    (kill-ring-save (point-min) (point-max))))

;; Advice functions
;; ------------------------------

; Keep track of where we were when we start paging around
(defadvice scroll-up-command (around my-scroll-up-marker activate)
  "Track last point before scrolling down the page"
  (push-mark (point) nil)
  ad-do-it)

(defadvice scroll-down-command (around my-scroll-down-marker activate)
  "Track last point before scrolling up the page"
  (push-mark (point) nil)
  ad-do-it)

; Or just set this
(setq scroll-preserve-screen-position t)

; Use my faux-screen library
(add-to-list 'load-path (expand-file-name "~/repos/faux-screen/"))
(setq faux-screen-num-terminals 10
      faux-screen-keymap-prefix (kbd "C-\\")
      faux-screen-terminal-ps1 "(\\[\\e[1;36m\\]%s\\[\\e[0m\\]) \\W $ ")
(require 'faux-screen)
(faux-screen-global-mode)
(global-set-key [C-next]  'faux-screen-next-dwim)
(global-set-key [C-prior] 'faux-screen-prev-dwim)
(faux-screen-terminals)

; Setup a utility terminal to be used in ad-hoc situations using the
; currnet default-directory location
(global-set-key (kbd "<f12>")
                (lambda ()
                  (interactive)
                  (funcall (faux-screen-utility-terminal "Utility") default-directory)))

; integrate with projectile
(defun jsm/projectile-shell ()
  (interactive)
  (funcall (faux-screen-utility-terminal "prj") (projectile-project-root)))
(define-key projectile-command-map (kbd "$") 'jsm/projectile-shell)

;; Recentf Support
;; ------------------------------
;; get rid of `find-file-read-only' and replace it with something
;; more useful.
(global-set-key (kbd "C-x C-r") 'ido-recentf-open)

;; enable recent files mode.
(recentf-mode t)

; 50 files ought to be enough.
(setq recentf-max-saved-items 50)

; Ignore certain files from the exclusive recentf-list
(add-to-list 'recentf-exclude "^/tmp/.*html$")
(add-to-list 'recentf-exclude "^/tmp/org")
(add-to-list 'recentf-exclude (expand-file-name "~/Maildir"))

(defun ido-recentf-open ()
  "Use `ido-completing-read' to \\[find-file] a recent file"
  (interactive)
  (if (find-file (ido-completing-read "Find recent file: " recentf-list))
      (message "Opening file...")
    (message "Aborting")))

;; Capture my work window configuration and be able to switch back to it easily
;; ------------------------------
;; TODO

;; Show me bad whitespace
;; ------------------------------
(require 'whitespace)
(setq whitespace-style '(face tabs trailing indentation space-before-tab space-after-tab)
      whitespace-global-modes '(python-mode c-mode c++-mode))
(global-whitespace-mode 1)
; (face tabs spaces trailing lines space-before-tab newline indentation empty space-after-tab space-mark tab-mark newline-mark)


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
