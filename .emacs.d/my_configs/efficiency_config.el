; -*- emacs-lisp -*-

; General custom config to help efficiency within the Editor. So, this
; could be many different things from project navigation, to custom
; function with key bindings, etc.

;; Using ace-jump
;; ------------------------------
(require 'ace-jump-mode)
(global-set-key (kbd "C-c SPC") 'ace-jump-mode)

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

;; Improved IDO match support
;; ------------------------------
(require 'flx-ido)
(flx-ido-mode 1)
;; Disable default ido faces to see flx highlights
(setq ido-use-faces nil)
; Finally, with the improved highlighting, it is also nice to view matches vertically
(ido-vertical-mode)

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
(projectile-global-mode)
; (setq projectile-tags-command "gtags %s") ; Was "ctags -Re %s"

;; Ack support with ack-and-a-half
;; ------------------------------
(require 'ack-and-a-half)
(defalias 'ack 'ack-and-a-half)
(defalias 'ack-same 'ack-and-a-half-same)
(defalias 'ack-find-file 'ack-and-a-half-find-file)
(defalias 'ack-find-file-same 'ack-and-a-half-find-file-same)

;; Markdown Helper routine
;; ------------------------------
(defun jsm:markdown-preview ()
  "Preview the markdown file in a new browser tab"
  (interactive)
  (let ((my-filename (buffer-file-name))
        (html-filename (concat (buffer-file-name) ".html")))
    (shell-command (concat "/usr/bin/markdown_py < " my-filename " > " html-filename) nil nil)
    (browse-url (concat "file://" html-filename))))
(add-hook 'markdown-mode-hook
          '(lambda () (define-key markdown-mode-map (kbd "<f12>") 'jsm:markdown-preview)))
