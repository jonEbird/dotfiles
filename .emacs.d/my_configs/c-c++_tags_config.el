; -*- emacs-lisp -*-

; ----------------------------------------------------------------------
; c-mode configs
; ----------------------------------------------------------------------
; Set my coding style for working with other Linux source
(add-hook 'c-mode-hook
          '(lambda ()
             (c-set-style "linux")))

; ----------------------------------------------------------------------
; c++-mode configs
; ----------------------------------------------------------------------
; Starting by adding an excerpt seen from:
; http://stackoverflow.com/questions/8549351/c11-mode-or-settings-for-emacs/12934513#12934513
(require 'font-lock)

(defun --copy-face (new-face face)
  "Define NEW-FACE from existing FACE."
  (copy-face face new-face)
  (eval `(defvar ,new-face nil))
  (set new-face new-face))

(--copy-face 'font-lock-label-face  ; labels, case, public, private, proteced, namespace-tags
             'font-lock-keyword-face)
(--copy-face 'font-lock-doc-markup-face ; comment markups such as Javadoc-tags
             'font-lock-doc-face)
(--copy-face 'font-lock-doc-string-face ; comment markups
             'font-lock-comment-face)

(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)

(add-hook 'c++-mode-hook
          '(lambda()
             ;; In theory, we could place some regexes into `c-mode-common-hook'. But note that their
             ;; evaluation order matters.
             (font-lock-add-keywords
              nil '(;; complete some fundamental keywords
                    ("\\<\\(void\\|unsigned\\|signed\\|char\\|short\\|bool\\|int\\|long\\|float\\|double\\)\\>" . font-lock-keyword-face)
                    ;; add the new C++11 keywords
                    ("\\<\\(alignof\\|alignas\\|constexpr\\|decltype\\|noexcept\\|nullptr\\|static_assert\\|thread_local\\|override\\|final\\)\\>" . font-lock-keyword-face)
                    ("\\<\\(char[0-9]+_t\\)\\>" . font-lock-keyword-face)
                    ;; PREPROCESSOR_CONSTANT
                    ("\\<[A-Z]*_[A-Z_]+\\>" . font-lock-constant-face)
                    ;; hexadecimal numbers
                    ("\\<0[xX][0-9A-Fa-f]+\\>" . font-lock-constant-face)
                    ;; integer/float/scientific numbers
                    ("\\<[\\-+]*[0-9]*\\.?[0-9]+\\([ulUL]+\\|[eE][\\-+]?[0-9]+\\)?\\>" . font-lock-constant-face)
                    ;; user-defined types (customizable)
                    ("\\<[A-Za-z_]+[A-Za-z_0-9]*_\\(type\\|ptr\\)\\>" . font-lock-type-face)
                    ;; some explicit typenames (customizable)
                    ("\\<\\(xstring\\|xchar\\)\\>" . font-lock-type-face)
                    ))
             ) t)

(defun my-c-mode-common-hook-func ()
  "Function to be called when entering into c-mode."
  (interactive)
  (when (and (require 'auto-complete nil t) (require 'auto-complete-config nil t))
    (auto-complete-mode t)
    (make-local-variable 'ac-sources)
    (setq ac-auto-start 2)
    (setq ac-sources '(ac-source-words-in-same-mode-buffers
                       ac-source-dictionary ac-source-gtags))
    ;; (when (require 'auto-complete-etags nil t)
    ;;   (add-to-list 'ac-sources 'ac-source-etags)
    ;;   (setq ac-etags-use-document t))
    (when (require 'auto-complete-clang nil t)
      (add-to-list 'ac-sources 'ac-source-clang))))

(add-hook 'c-mode-common-hook 'my-c-mode-common-hook-func)

; Auto-complete support for C++
(require 'auto-complete-clang)
(define-key c++-mode-map (kbd "C-M-<return>") 'ac-complete-clang)

; ----------------------------------------------------------------------
; Source Code Navigation - Aka Tags
; ----------------------------------------------------------------------
;; I have gtags installed via ELPA package system
; Lets gtags know that we're okay with their suggestive key bindings
(setq gtags-suggested-key-mapping t
      gtags-auto-update t)

(require 'gtags)
(add-hook 'c-mode-common-hook '(lambda () (gtags-mode 1)))
; gtags-find-tag (C-c t) - goes to function def
; gtags-find-rtag (C-c r) - references to function (caller/callee?)
; gtags-find-symbol - locate tokens that are not in the GTAGS file

; M-. find-tag
; C-M-. find-tag-regexp
; M-* pop-tag-mark - Pop back to where M-. was last invoked
; M-, tags-loop-continue - continues a tags-search or tags-query-replace

(add-hook 'gtags-select-mode-hook
  '(lambda ()
     (setq hl-line-face 'underline)
     (hl-line-mode 1)))

; Update the Projectile command for generating tags
(setq projectile-tags-command "gtags")

; (gtags-visit-rootdir "/usr/src/linux-2.6.21")

;; etags-select extensions
;; (require 'etags-select)
;; (require 'etags-table)
;; ;; (global-set-key "\M-?" 'etags-select-find-tag-at-point)
;; ;; (global-set-key "\M-." 'etags-select-find-tag)
;; ; This helps you automatically search and find the TAGS file
;; (setq etags-table-search-up-depth 10)
;; ; Also nice to be able to explictly remember the interactive function to visit another TAGS file
;; (defalias 'tags-visit-table 'visit-tags-table)

;; Taken from http://www.emacswiki.org/emacs/EtagsSelect
(defun my-ido-find-tag ()
  "Find a tag using ido"
  (interactive)
  (tags-completion-table)
  (let (tag-names)
    (mapatoms (lambda (x)
                (push (prin1-to-string x t) tag-names))
              tags-completion-table)
    (etags-select-find (ido-completing-read "Tag: " tag-names))))
;; Commented out for now. Nice but too slow for huge TAGS files such as for the Linux kernel
; (global-set-key (kbd "M-.") 'my-ido-find-tag)
